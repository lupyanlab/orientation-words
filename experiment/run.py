#!/usr/bin/env python
import copy
from UserDict import UserDict
from UserList import UserList

import unipath
import pandas
from numpy import random
import yaml

try:
    import pyo
except ImportError:
    print 'pyo not installed!'
from psychopy import prefs
prefs.general['audioLib'] = ['pyo', ]
from psychopy import sound

from psychopy import visual, core, event

from labtools import DynamicMask
from labtools.trials_functions import expand, extend, add_block, smart_shuffle
from labtools.psychopy_helper import get_subj_info, load_sounds, load_images

class Participant(UserDict):
    """ Store participant data and provide helper functions. """
    DATA_DIR = 'data'
    DATA_DELIMITER = ','

    def __init__(self, **kwargs):
        """ Standard dict constructor.

        Saves _order if provided. Raises an AssertionError if _order
        isn't exhaustive of kwargs.
        """
        self._data_file = None
        self._order = kwargs.pop('_order', kwargs.keys())

        correct_len = len(self._order) == len(kwargs)
        kwargs_in_order = all([kwg in self._order for kwg in kwargs])
        assert correct_len & kwargs_in_order, "_order doesn't match kwargs"

        self.data = dict(**kwargs)

    @property
    def data_file(self):
        if not unipath.Path(self.DATA_DIR).exists():
            unipath.Path(self.DATA_DIR).mkdir()

        if not self._data_file:
            data_file_name = '{subj_id}.csv'.format(**self)
            self._data_file = unipath.Path(self.DATA_DIR, data_file_name)
        return self._data_file

    def write_header(self, trial_col_names):
        """ Writes the names of the columns and saves the order. """
        self._col_names = self._order + trial_col_names
        self._write_line(self.DATA_DELIMITER.join(self._col_names))

    def write_trial(self, trial):
        assert self._col_names, 'write header first to save column order'
        trial_data = dict(self)
        trial_data.upandasate(trial)
        row_data = [str(trial_data[key]) for key in self._col_names]
        self._write_line(self.DATA_DELIMITER.join(row_data))

    def _write_line(self, row):
        with open(self.data_file, 'a') as f:
            f.write(row + '\n')


class Trials(UserList):
    STIM_DIR = 'stimuli'
    COLUMNS = [
        # Trial columns
        'block',
        'block_type',
        'trial',

        # Stimuli columns
        'cue',
        'cue_type',
        'mask_type',
        'response_type',
        'target',
        'target_loc',
        'correct_response',

        # Response columns
        'response',
        'rt',
        'is_correct',
    ]

    @classmethod
    def make(cls, **kwargs):
        seed = kwargs.get('seed')
        prng = random.RandomState(seed)

        # Balance within subject variables
        trials = pandas.DataFrame({'mask_type': ['nomask', 'mask']})
        trials = expand(trials, name='cue_type', values=['valid', 'invalid'],
                        ratio=0.75, seed=seed)
        trials = expand(trials, name='response_type', values=['pic', 'word'],
                        ratio=0.75, seed=seed)

        # Set length of experiment
        trials = extend(trials, max_length = 320)

        # Determine target category on every trial
        categories_csv = unipath.Path(cls.STIM_DIR, 'categories.csv')
        categories = pandas.read_csv(categories_csv).category
        trials['target'] = prng.choice(categories, len(trials), replace=True)
        trials['target_loc'] = prng.choice(['left', 'right'], len(trials),
                                           replace=True)

        def pick_cue(trial):
            if trial['cue_type'] == 'valid':
                return trial['target']
            else:
                distractors = list(categories)
                distractors.remove(trial['target'])
                return prng.choice(distractors)

        trials['cue'] = trials.apply(pick_cue, axis=1)

        response_map = dict(valid='match', invalid='mismatch')
        def pick_correct_response(trial):
            if trial['response_type'] == 'pic':
                return trial['target_loc']
            else:
                return response_map[trial['cue_type']]

        trials['correct_response'] = trials.apply(pick_correct_response, axis=1)

        # Add block
        trials = add_block(trials, size=60, start=0, seed=seed)
        trials['block_type'] = 'test'

        # Add practice trials
        num_practice = 12
        practice_ix = prng.choice(trials.index, num_practice)
        practice_trials = trials.ix[practice_ix, ]
        trials.drop(practice_ix, inplace=True)

        practice_trials['block_type'] = 'practice'
        practice_trials['block'] = -1
        trials = pandas.concat([practice_trials, trials])

        # Shuffle
        trials = smart_shuffle(trials, col='target', block='block', seed=seed)

        # Enumerate trials
        trials['trial'] = range(len(trials))

        # Add blank columns for response variables
        for c in ['response', 'rt', 'is_correct']:
            trials[c] = ''

        return cls(trials.to_dict('record'))

    def write_trials(self, trials_csv):
        trials = pandas.DataFrame.from_records(self)
        trials = trials[self.COLUMNS]
        trials.to_csv(trials_csv, index=False)

    def iter_blocks(self, key='block'):
        """ Yield blocks of trials. """
        block = self[0][key]
        trials_in_block = []
        for trial in self:
            if trial[key] == block:
                trials_in_block.append(trial)
            else:
                yield trials_in_block
                block = trial[key]
                trials_in_block = []


class Experiment(object):
    STIM_DIR = 'stimuli'

    def __init__(self, settings_yaml='settings.yaml', texts_yaml='texts.yaml'):
        with open(settings_yaml) as f:
            settings = yaml.load(f)
        self.layout = settings.pop('layout')
        self.positions = self.layout.pop('positions')
        self.waits = settings.pop('waits')
        self.response_keys = settings.pop('response_keys')

        with open(texts_yaml) as f:
            self.texts = yaml.load(f)

        self.win = visual.Window(fullscr=True, units='pix')

        text_kwargs = dict(
            win=self.win,
            font='Consolas',
            height=60,
            color='black'
        )
        self.fix = visual.TextStim(text='+', **text_kwargs)
        self.prompt = visual.TextStim(text='?', **text_kwargs)

        pic_size = self.layout['pic_size']
        frame_kwargs = dict(
            win=self.win,
            lineColor='black',
            lineWidth=2.0,
            fillColor=None,
            width=pic_size[0] + 10,
            height=pic_size[0] + 10,
        )
        self.frames = [visual.Rect(pos=pos, **frame_kwargs)
                       for pos in self.positions.values()]

        self.cues = load_sounds(unipath.Path(self.STIM_DIR, 'cues'))


        mask_kwargs = dict(
            win=self.win,
            size=pic_size,
        )
        self.masks = [DynamicMask(pos=pos, **mask_kwargs)
                      for pos in self.positions.values()]

        # Targets
        image_kwargs = dict(
            win=self.win,
            size=pic_size,
            # pos is set in run_trial
        )
        self.pics = load_images(unipath.Path(self.STIM_DIR, 'pics'),
                                **image_kwargs)

        self.word = visual.TextStim(**text_kwargs)

        self.timer = core.Clock()

        feedback_dir = unipath.Path(self.STIM_DIR, 'feedback')
        self.feedback = {}
        self.feedback[0] = sound.Sound(unipath.Path(feedback_dir, 'buzz.wav'))
        self.feedback[1] = sound.Sound(unipath.Path(feedback_dir, 'bleep.wav'))

    def run_trial(self, trial):
        # Cue files are named "alligator-1", "alligator-2".
        # This list contains the cue files that match the
        # cue in the trial, e.g. "alligator"
        cue_versions = [snd for n, snd in self.cues.items()
                        if n.find(trial['cue']) == 0]
        cue = random.choice(cue_versions)
        cue_dur = cue.getDuration()

        target_stims = []
        if trial['response_type'] == 'pic':
            base = self.pics[trial['target']]
            assert trial['target_loc'] in ['left', 'right']
            for pos in ['left', 'right']:
                pic = copy.copy(base)
                pic.setPos(self.positions[pos])
                if trial['target_loc'] != pos:
                    pic.setOri(180.0)
                target_stims.append(pic)
        elif trial['response_type'] == 'word':
            self.word.setText(trial['target'])
            self.word.setPos(self.positions[trial['target_loc']])
            target_stims.append(self.word)
        else:
            raise NotImplementedError

        soa = self.waits['cue_onset_to_target_onset']
        cue_offset_to_target_onset = soa - cue_dur

        stim_during_cue = []
        if trial['mask_type'] == 'mask':
            stim_during_cue.extend(self.masks)

        # Begin trial presentation
        # ------------------------
        self.fix.autoDraw = True
        for frame in self.frames:
            frame.autoDraw = True
        self.win.flip()
        core.wait(self.waits['fixation_duration'])

        self.timer.reset()
        cue.play()
        while self.timer.getTime() < cue_dur:
            [stim.draw() for stim in stim_during_cue]
            self.win.flip()
            core.wait(0.01)

        self.win.flip()
        core.wait(cue_offset_to_target_onset)

        for target in target_stims:
            target.draw()
        self.timer.reset()
        self.win.flip()
        core.wait(self.waits['target_duration'])

        self.fix.autoDraw = False
        for frame in self.frames:
            frame.autoDraw = False
        self.prompt.draw()
        self.win.flip()
        response = event.waitKeys(maxWait=self.waits['response_window'],
                                  keyList=self.response_keys.keys(),
                                  timeStamped=self.timer)
        self.win.flip()
        # ----------------------
        # End trial presentation

        try:
            key, rt = response[0]
        except TypeError:
            rt = self.waits['response_window']
            response = 'timeout'
        else:
            response = self.response_keys[key]

        is_correct = int(response == trial['correct_response'])

        self.feedback[is_correct].play()

        core.wait(self.waits['iti'])

        trial['response'] = response
        trial['rt'] = rt * 1000
        trial['is_correct'] = is_correct

        return trial

def main():
    participant_data = get_subj_info(
        'gui.yaml',
        # check_exists is a simple function to determine if the data file
        # exists, provided subj_info data. Here it's used to check for
        # uniqueness in subj_ids when getting info from gui.
        check_exists=lambda subj_info:
            Participant(**subj_info).data_file.exists()
    )

    participant = Participant(**participant_data)
    trials = Trials.make(**participant)

    experiment = Experiment()
    experiment.show_screen('instructions')

    participant.write_header(trials.COLUMNS)

    for block in trials.iter_blocks():
        block_type = block[0]['block_type']

        for trial in block:
            trial_data = experiment.run_trial(trial)
            participant.write_trial(trial_data)

        experiment.show_screen(block_type)

    experiment.show_screen('end')
    import webbrowser
    webbrowser.open(experiment.survey_url.format(subj_id='TEST_SUBJ', computer='TEST_COMPUTER'))


if __name__ == '__main__':
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument('command', choices=['main', 'maketrials', 'single'],
                        nargs='?', default='main')

    default_trial_options = dict(
        cue='elephant',
        response_type='pic',
        target='elephant',
        target_loc='left',
        mask_type='mask',
        correct_response='left',
    )

    for name, default in default_trial_options.items():
        parser.add_argument('--%s' % name, default=default)

    args = parser.parse_args()

    if args.command == 'maketrials':
        trials = Trials.make()
        trials.write_trials('sample_trials.csv')
    elif args.command == 'single':
        trial = dict(default_trial_options)
        for name in default_trial_options:
            if hasattr(args, name):
                trial[name] = getattr(args, name)

        experiment = Experiment()
        trial_data = experiment.run_trial(trial)

        import pprint
        pprint.pprint(trial_data)
    elif args.command == 'survey':
        experiment = Experiment()
        import webbrowser
        webbrowser.open(experiment.survey_url.format(subj_id='TEST_SUBJ', computer='TEST_COMPUTER'))
    else:
        main()
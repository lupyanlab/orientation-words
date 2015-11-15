#!/usr/bin/env python
from UserDict import UserDict
from UserList import UserList

import unipath
import pandas
from numpy import random

from labtools.trials_functions import expand, extend, add_block, smart_shuffle


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
        'cue_file',
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

        # cue_file is determined at run time
        trials['cue_file'] = ''

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
    pass


def main():
    print 'running experiment'

if __name__ == '__main__':
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument('command', choices=['main', 'trials', 'test'],
                        nargs='?', default='main')

    args = parser.parse_args()

    if args.command == 'trials':
        trials = Trials.make()
        trials.write_trials('sample_trials.csv')
    elif args.command == 'test':
        experiment = Experiment()
        trial_data = experiment.run_trial()
        import pprint
        pprint.pprint(trial_data)
    else:
        main()

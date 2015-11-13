#!/usr/bin/env python
from UserDict import UserDict
from UserList import UserList

import unipath
import pandas

from labtools.trials_functions import expand


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
    def make(cls):
        # Balance within subject variables
        trials = pandas.DataFrame({'mask_type': ['nomask', 'mask']})
        trials = expand(trials, name='cue_type', values=['valid', 'invalid'],
                        ratio=0.75)
        trials = expand(trials, name='response_type', values=['pic', 'word'],
                        ratio=0.75)

        # Add picture
        trials['cue'] = ''
        trials['target'] = ''
        trials['target_loc'] = ''
        trials['correct_response'] = ''

        # Add block
        trials['block'] = ''
        trials['block_type'] = ''
        trials['trial'] = range(len(trials))

        # Add blank columns for response variables
        for c in ['response', 'rt', 'is_correct']:
            trials[c] = ''

        return cls(trials.to_dict('record'))

    def write_trials(self, trials_csv):
        trials = pandas.DataFrame.from_records(self)
        print trials.columns
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

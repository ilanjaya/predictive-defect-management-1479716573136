# -*- coding: utf-8 -*-
"""
@author: nilesh

Tasks: Completed
10. Convert ensamble function to class
71. Perform only 0 cv fold (no cv)
        http://stackoverflow.com/questions/29503689/how-to-run-gridsearchcv-without-cross-validation
        ShuffleSplit(test_size=0.20, n_iter=1, random_state=0)

Tasks: Pending
01. Log to file and console
03. Implement concept of run / batch
07. Estimate time remaining for batch run
09. View progress of run
20. Increase itteration automaticaly when ConvergenceWarning is desplayed
70. Fix elasticnet error (may be in predict funtion)
72. Implement random_state to get same results in every run
73. keras sklern classifier should take cv data as validation data
74. Implement model stacking
"""

from __future__ import print_function
import os
import sys
import pandas as pd
import simplejson
import numpy as np
import warnings
warnings.filterwarnings("ignore", category=DeprecationWarning) 
import pickle
from multiprocessing import cpu_count
from datetime import datetime
import itertools
from configobj import ConfigObj
import glob
import random
from joblib import Parallel, delayed
from sklearn.externals import joblib
import copy
import zipfile
import platform
import imp

from sklearn.pipeline import Pipeline, FeatureUnion
from sklearn.decomposition import PCA
from sklearn.preprocessing import PolynomialFeatures, OneHotEncoder, Normalizer
from sklearn.feature_extraction import DictVectorizer
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.decomposition import TruncatedSVD
from sklearn.base import BaseEstimator, TransformerMixin
from sklearn.feature_selection import SelectKBest, SelectFromModel

from sklearn.cross_validation import ShuffleSplit
from sklearn.cross_validation import train_test_split
from sklearn.grid_search import _CVScoreTuple, GridSearchCV
from sklearn.metrics import accuracy_score, make_scorer, cohen_kappa_score

from sklearn.multiclass import OutputCodeClassifier
from sklearn.linear_model import LogisticRegression, ElasticNet, SGDRegressor
from sklearn.linear_model import SGDClassifier
from sklearn.svm import SVC
from sklearn.neural_network import BernoulliRBM
from sklearn.ensemble import RandomForestClassifier, RandomForestRegressor
from sklearn.ensemble import ExtraTreesClassifier

# from xgboost.sklearn import XGBClassifier
from keras.preprocessing.text import Tokenizer
from keras.preprocessing import sequence
from keras import backend as K
from keras.models import Sequential
from keras.layers.core import Dense, Activation
from keras.wrappers.scikit_learn import KerasClassifier
from keras.layers import Dropout, Lambda, Embedding, LSTM, Convolution1D
from keras.models import load_model
from keras.callbacks import ModelCheckpoint, EarlyStopping, TensorBoard
from keras.callbacks import RemoteMonitor, ReduceLROnPlateau


# User Inputs -----------------------------------------------------------------
path_base = os.path.dirname(os.path.dirname(os.path.realpath(__file__))) 
fl_word_vectors = 'ab_glove.6B.300d.txt'
problem_type = 'multinom' # alternatives: regression, binon, multinom
func_target_transform = lambda x: x
func_target_reverse_transform = lambda x: x
round_digits = 4
cv_test_size = 0.1764
def scoring_function(x, y): 
      return cohen_kappa_score(func_target_reverse_transform(x), 
                               func_target_reverse_transform(y)) 
scoring_function_scorer = make_scorer(scoring_function, greater_is_better = True,
                                      needs_proba = False)
def scoring_function2(x, y): 
    return accuracy_score(func_target_reverse_transform(x), 
                               func_target_reverse_transform(y)) 

    
n_threads = cpu_count() - 1
if platform.uname()[1] == 'AIIGULCE001':
    n_threads_max = 40
else:
    n_threads_max = 8
save_classification_report = True

# Set base path ---------------------------------------------------------------
os.chdir(path_base)
sys.path.append(path_base)


# Directory Structure ---------------------------------------------------------
class DirStructure():
    def __init__(self, fl_config = 'config.ini'):
        config = ConfigObj(fl_config)
        dir_structure = config['dir_structure']
        self.user_inputs = dir_structure ['fld_user_inputs']
        self.model_meta_data = dir_structure ['fld_model_meta_data']
        self.inp_data = dir_structure ['fld_inp_data']
        self.inp_data_pre_proc = dir_structure['fld_inp_data_pre_proc']
        self.data_featur_eng = dir_structure['fld_data_featur_eng']
        self.insights = dir_structure['fld_insights']
        self.model_pipeline = dir_structure['fld_model_pipeline']
        self.model_param_tun = dir_structure['fld_model_param_tun']
        self.model_scoring = dir_structure ['fld_model_scoring']
        self.model_result_summary = dir_structure['fld_model_result_summary']
        self.ensm_pipeline = dir_structure['fld_ensm_pipeline']
        self.ensm_param_tun = dir_structure['fld_ensm_param_tun']   
        self.ensm_scoring = dir_structure['fld_ensm_scoring']
        self.ensm_result_summary = dir_structure['fld_ensm_result_summary']
        self.model_verification = dir_structure ['fld_model_verification']
        self.unit_tests = dir_structure ['fld_unit_tests']
        self.scripts = dir_structure ['fld_scripts']
        self.docs = dir_structure ['fld_docs']
        self.logs = dir_structure ['fld_logs']

    def create_fld_if_not_exist(self, fld):
        if not os.path.exists(os.path.join(path_base, fld)):
            os.makedirs(os.path.join(path_base, fld))
            print('Created folder:', fld)

    def get_path(self, *paths):
        return os.path.join(path_base, *paths)

fld = DirStructure('config.ini')


# Logging: --------------------------------------------------------------------
'''
logging.basicConfig(level=logging.DEBUG,
                    stream=sys.stdout,
                    format='%(asctime)s %(name)-12s %(levelname)-8s %(message)s',
                    datefmt='%Y-%m-%d %H:%M:%S',
                    filename='r_logs/job.log',
                    filemode='a+')
console = logging.StreamHandler()
console.setLevel(logging.INFO)
formatter = logging.Formatter('%(name)-12s: %(levelname)-8s %(message)s')
console.setFormatter(formatter)
logging.getLogger('').addHandler(console)
logging.info('Starting New Job Run')
'''

# Classe: Models --------------------------------------------------------------


# Classes ---------------------------------------------------------------------
class Data:
    def __init__(self, fl_inp_train = None, fl_inp_test = None, pd_or_np = 'pd',
                 fl_submission_input = None,
                 fld_data = fld.inp_data, fl_pkl = 'a_data.pkl'):
        self.fl_inp_train = fl_inp_train
        self.fl_inp_test = fl_inp_test
        self.fld_data = fld_data
        self.fl_pkl = fl_pkl
        self.fl_submission_input = fl_submission_input,
        
        self.n_train = None
        self.n_test = None
        self.idx_train = None
        self.idx_valid = None
        self.idx_test = None
        self.valid_as_pct_of_train = None

        self.fs_target = []
        self.fs_id = []
        self.fs_ind = []
        self.fs_categorical = []
        self.fs_remove = []
        
        self.pd_or_np = pd_or_np        
        self.df = None
        self.X = None
        self.y = None
        self.id = None

    def run(self, options = ['create_save', 'pre_process', 'save', 'describe', 
                             'explore', 'load']):
        if 'create_save' in options:
            self.create_save_data()
        if 'pre_process' in options:
            self.pre_process(options = ['remove_constant_cols',
                                          'remove_duplicated_cols'])
        if 'describe' in options:
            self.describe()
        if 'explore' in options:
            self.explore()
        if 'load' in options:
            self.load()
    
    def create_save_data(self, fld = fld):
        train = pd.read_csv(fld.get_path(self.fld_data, self.fl_inp_train)) 
        test = pd.read_csv(fld.get_path(self.fld_data, self.fl_inp_test))    
        df = pd.concat([train, test], ignore_index = True)
        '''
        train['sent'] = train['sent'].apply(lambda l : [1 if l == 1 else 0])
        test['sent'] = test['sent'].apply(lambda l : [1 if l == 1 else 0])
        '''
        self.df = df
        self.n_train = train.shape[0]
        self.n_test = test.shape[0] 
        with open(fld.get_path(self.fld_data, self.fl_pkl), 'wb') as fl:
            pickle.dump(self.__dict__, fl, pickle.HIGHEST_PROTOCOL)

    def save(self, fld_destination, fl, fld = fld):
        with open(fld.get_path(fld_destination, fl), 'wb') as f:
            pickle.dump(self.__dict__, f, pickle.HIGHEST_PROTOCOL)
        return None
        
    def load(self, fld_destination, fl, fld = fld):
        with open(fld.get_path(fld_destination, fl), 'rb') as f:
            tmp_dict = pickle.load(f)    
        self.__dict__.update(tmp_dict)
        return None
        
    def load_pre_processed(self, fl_train, fl_test, fld = fld, 
                           nrows_train = None, nrows_test = None):
        train = pd.read_csv(fld.get_path(fld.inp_data_pre_proc, fl_train), 
                            nrows = nrows_train)
        test = pd.read_csv(fld.get_path(fld.inp_data_pre_proc, fl_test),
                           nrows = nrows_test)
        df = pd.concat([train, test], ignore_index = True)
        if 'Unnamed: 0' in df.columns:
            del df['Unnamed: 0']
        self.df = df
        self.n_train = train.shape[0]
        self.n_test = test.shape[0] 
        return None
    
    def load_pre_processed_pkl_part(self, fld_train, fld_test, fld = fld, 
                           nrows_train = None, nrows_test = None):
        train_list = []       
        path = os.path.join(fld.get_path(fld.inp_data_pre_proc, fld_train), '*.pkl')
        files = glob.glob(path)
        for fl in files:
            with open(fl, 'rb') as fl:
                train_list.append(pickle.load(fl))
        train = pd.concat(train_list)
        if nrows_train is not None:
            train = train.iloc[random.sample(range(len(train)), nrows_train)]

        test_list = []       
        path = os.path.join(fld.get_path(fld.inp_data_pre_proc, fld_test), '*.pkl')
        files = glob.glob(path)
        for fl in files:
            with open(fl, 'rb') as fl:
                test_list.append(pickle.load(fl))
        test = pd.concat(test_list)
        if nrows_test is not None:
            test = test.iloc[random.sample(range(len(test)), nrows_test)]
        
        df = pd.concat([train, test], ignore_index = True)
        self.df = df
        self.n_train = train.shape[0]
        self.n_test = test.shape[0] 
        return None
    
    def load_pre_processed_np(self, fld_pre_proc, 
                     fld = fld, nrows_train = None, nrows_test = None):
                         
        X_train = np.load(fld.get_path(fld.inp_data_pre_proc, fld_pre_proc['fld'], 
                         fld_pre_proc['fl_X_train']))
        y_train = np.load(fld.get_path(fld.inp_data_pre_proc, fld_pre_proc['fld'], 
                         fld_pre_proc['fl_y_train']))
        X_test = np.load(fld.get_path(fld.inp_data_pre_proc, fld_pre_proc['fld'], 
                         fld_pre_proc['fl_X_test']))
        y_test = np.load(fld.get_path(fld.inp_data_pre_proc, fld_pre_proc['fld'], 
                         fld_pre_proc['fl_y_test']))
        id_train = np.load(fld.get_path(fld.inp_data_pre_proc, fld_pre_proc['fld'], 
                         fld_pre_proc['fl_id_train']))
        id_test = np.load(fld.get_path(fld.inp_data_pre_proc, fld_pre_proc['fld'], 
                         fld_pre_proc['fl_id_test']))
        self.X = np.concatenate((X_train, X_test))
        self.y = np.concatenate((y_train, y_test))
        self.id = np.concatenate((id_train, id_test))
        self.n_train = X_train.shape[0]
        self.n_test = X_test.shape[0] 
        return None
    
    def create_from_np_objects(self, X_train, y_train, X_valid, y_valid, 
                               X_test, y_test,
                               id_train = None, id_test = None,
                               fld = fld, nrows_train = None, nrows_test = None):
        print('X shape', X_train.shape, X_valid.shape, X_test.shape)
        self.n_train = X_train.shape[0]
        self.n_valid = X_valid.shape[0]
        self.n_test = X_test.shape[0]
        id_train = range(self.n_train + self.n_valid)
        id_test = [i + self.n_train + self.n_valid for i in range(self.n_test)]
        print(self.n_train, self.n_valid, self.n_test, id_train[-1], id_test[-1])
        self.X = np.concatenate((X_train, X_valid, X_test))
        self.y = np.concatenate((y_train, y_valid, y_test))
        self.id = np.concatenate((id_train, id_test))
        return None
        
    def pre_process(self, options = ['inpute_na',
                                     'remove_constant_cols', 
                                     'remove_duplicated_cols', 
                                     'transform_log']):
        self.df.srch_ci_year = self.df.srch_ci.apply(lambda s: str(s)[0:3])
        self.df.drop('srch_ci', axis=1, inplace=True)
        self.df.srch_co_year = self.df.srch_co.apply(lambda s: str(s)[0:3])
        self.df.drop('srch_co', axis=1, inplace=True)

        self.df.drop('date_time', axis=1, inplace=True)
        print(self.fs_ind)
        print(type(self.fs_ind))
        self.fs_ind.remove('srch_ci')
        self.fs_ind.remove('srch_co')
        self.fs_ind.remove('date_time')
        
        if 'inpute_na' in options:
            for col in self.fs_ind:
                tt = self.df[col].values
                tt[np.isnan(tt)] = np.nanmean(tt)
                self.df[col] = tt
        
        if 'remove_constant_cols' in options:
            # remove constant columns (std = 0)
            for col in self.fs_ind:
                if self.df[col].std() == 0:
                    self.fs_remove.append(col)
        
        if 'remove_duplicated_cols' in options:
            cols = self.fs_ind
            for i in range(len(cols)-1):
                v = self.df[cols[i]].values
                for j in range(i+1,len(cols)):
                    if np.array_equal(v,self.df[cols[j]].values):
                        self.fs_remove.append(cols[j])  
        self.fs_remove = list(set(self.fs_remove))

        if 'transform_log' in options:
            # transformation: log
            for col in self.fs_ind:
                if np.min(self.df[col]) < 0:
                    self.df[col] = self.df[col].add(1 - np.min(self.df[col]))
                    # print(col,':', np.min(self.df[col]))
                    self.df[col] = np.log(self.df[col])
    
    def transform_target(self):
        self.df[self.fs_target] = self.df[self.fs_target].apply(func_target_transform)
        return None
        
    def load_feature_engineered(self, fl_train, fl_test, fld = fld, 
                                nrows_train = None, nrows_test = None):
        train = pd.read_csv(fld.get_path(fld.data_featur_eng, fl_train), 
                            nrows = nrows_train)
        test = pd.read_csv(fld.get_path(fld.data_featur_eng, fl_test),
                           nrows = nrows_test)
        df = pd.concat([train, test], ignore_index = True)
        self.df = df
        self.n_train = train.shape[0]
        self.n_test = test.shape[0] 
    
    def describe(self):
        print(self.key_veriables())
        if self.pd_or_np == 'np':
            print('X:\n', self.X[0:2])
            print('y:\n', self.y[0:2])
        else:
            print(self.df.head())
    
    def explore(self):
        pass
    
    def create_ensamble_data(self, fld = fld):
        train, test = self.df.iloc[self.idx_train], self.df.iloc[self.idx_test]
        train = train[self.fs_id + self.fs_target]
        test = test[self.fs_id + self.fs_target]
        for dirpath, dirnames, filenames in os.walk(fld.get_path(fld.model_scoring)):
            for filename in [f for f in filenames if f.endswith("_train.csv")]:
                train = pd.merge(train, pd.read_csv(os.path.join(dirpath, filename)),
                           how = 'left', on = self.fs_id, copy = False)
                train.columns.values[-1] = '_'.join(os.path.splitext(filename)[0].split('_')[:-1])
        for dirpath, dirnames, filenames in os.walk(fld.get_path(fld.model_scoring)):
            for filename in [f for f in filenames if f.endswith("_test.csv")]:
                test = pd.merge(test, pd.read_csv(os.path.join(dirpath, filename)),
                           how = 'left', on = self.fs_id, copy = False)
                test.columns.values[-1] = '_'.join(os.path.splitext(filename)[0].split('_')[:-1])
        
        train.columns.values[1] = str(self.fs_target[0])
        test.columns.values[1] = str(self.fs_target[0])
        train.to_csv(fld.get_path(fld.ensm_scoring, 'train.csv'), index=False)
        test.to_csv(fld.get_path(fld.ensm_scoring, 'test.csv'), index=False)
        
        # dat = train.append(test)
        dat = pd.concat([train, test], ignore_index=True)
        #TODO: Sort creating problem when train index number is larger than test
        # dat = dat.sort_values(self.fs_id)
        dat = dat.reset_index(drop = True)
        '''
        dat = pd.merge(self.df.reset_index(drop = True), 
                       dat.reset_index(drop = True),
                       how = 'left', on = self.fs_id[0], copy = False)
        if self.fs_target[0] + '_y' in dat.columns:
            dat = dat.drop(self.fs_target[0] + '_y', axis = 1)
        dat.rename(columns = {self.fs_target[0] + '_x' : self.fs_target[0]}, 
                                  inplace = True)
        '''
        self.df = dat
        return None
    
    def load_ensamble_data(self, fl_train = fld.get_path(fld.ensm_scoring, 'train.csv'),
                           fl_test = fld.get_path(fld.ensm_scoring, 'test.csv'), 
                           fld = fld, nrows_train = None, nrows_test = None):
        train = pd.read_csv(fld.get_path(fld.inp_data_pre_proc, fl_train), 
                            nrows = nrows_train)
        test = pd.read_csv(fld.get_path(fld.inp_data_pre_proc, fl_test),
                           nrows = nrows_test)
        df = pd.concat([train, test], ignore_index = True)
        self.df = df
        self.n_train = train.shape[0]
        self.n_test = test.shape[0]
        return None
        
    def set_idx_train_valid_test(self, valid_as_pct_of_train = 0.2):
        self.valid_as_pct_of_train = valid_as_pct_of_train
        idx_train_valid = range(self.n_train)
        self.idx_test = [self.n_train + i for i in range(self.n_test)] 
        self.idx_train, self.idx_valid = \
            train_test_split(idx_train_valid, 
                             test_size=self.valid_as_pct_of_train,
                             random_state=111)
        cv = ShuffleSplit(len(self.idx_train), test_size=cv_test_size, 
                              n_iter=1, random_state=0)
        for train_index, test_index in cv:
            self.idx_cv = list(np.take(self.idx_train, test_index))
        return None
        
    def set_idx_train_valid_test_custom(self, n_train, n_valid, n_test):
        self.valid_as_pct_of_train = n_valid / (n_train + n_valid)
        self.idx_train = range(n_train)
        self.idx_valid = [n_train + i for i in range(n_valid)]
        self.idx_test = [n_train + n_valid + i for i in range(n_test)] 
        cv = ShuffleSplit(len(self.idx_train), test_size=cv_test_size, 
                              n_iter=1, random_state=0)
        for train_index, test_index in cv:
            self.idx_cv = list(np.take(self.idx_train, test_index))
        return None
        
    def set_fs(self, fs_target, fs_id, fs_ind, fs_categorical = None, 
               fs_numeric = None):
        self.fs_target = list(fs_target)
        self.fs_id = list(fs_id)
        if (fs_ind == 'all') | ('all' in fs_ind):
            fs_ind = list(set(self.df.columns) - \
                     set(self.fs_id + self.fs_target + self.fs_remove))
        self.fs_ind = fs_ind
        self.fs_categorical = fs_categorical
        self.fs_numeric = fs_numeric
        return None

    def fs_index(self, fs):
        idx = []
        for f in fs:
            idx.append(self.fs_ind.index(f))
        return idx
        
    def fs_categorical_position(self):
        l = []
        for x in self.fs_ind:
            if x in self.fs_categorical:
                l.append(1)
            else:
                l.append(0)
        return l
    
    def X_train(self):
        if self.pd_or_np == 'np':
            return self.X[self.idx_train]
        else:
            return self.df.iloc[self.idx_train][self.fs_ind].values

    def X_cv(self):
        if self.pd_or_np == 'np':
            return self.X[self.idx_cv]
        else:
            return self.df.iloc[self.idx_cv][self.fs_ind].values

    def X_valid(self):
        if self.pd_or_np == 'np':
            return self.X[self.idx_valid]
        else:
            return self.df.iloc[self.idx_valid][self.fs_ind].values

    def X_test(self):
        if self.pd_or_np == 'np':
            return self.X[self.idx_test]
        else:
            return self.df.iloc[self.idx_test][self.fs_ind].values

    def y_train(self):
        if self.pd_or_np == 'np':
            return self.y[self.idx_train]
        else:
            return self.df.iloc[self.idx_train][self.fs_target].values.ravel()
        
    def y_cv(self):
        if self.pd_or_np == 'np':
            return self.y[self.idx_cv]
        else:
            return self.df.iloc[self.idx_cv][self.fs_target].values.ravel()

    def y_valid(self):
        if self.pd_or_np == 'np':
            return self.y[self.idx_valid]
        else:
            return self.df.iloc[self.idx_valid][self.fs_target].values.ravel()

    def y_test(self):
        if self.pd_or_np == 'np':
            return self.y[self.idx_test]
        else:
            return self.df.iloc[self.idx_test][self.fs_target].values.ravel()

    def key_veriables(self):
        if self.pd_or_np == 'np':
            fs_all_count = self.X.shape[0]
        else:
            fs_all_count = len(self.df.columns)
        return {'n_train' : self.n_train, 'n_test' : self.n_test, 
                              'valid_as_pct_of_train' : \
                              self.valid_as_pct_of_train, 
                              'fs_ind_count': len(self.fs_ind),
                              'fs_all_count': fs_all_count,
                              'fs_remove_count': len(self.fs_remove),
                              'train_pct': round(1 - self.valid_as_pct_of_train, 
                                                 2)}


class Result():
    def __init__(self):
        self.score_train = None
        self.score_valid = None
        self.scroe_cv = None
        self.score_leader_board = None
        self.scores_grid = None
        self.score2_train = None
        self.score2_valid = None
        self.score2_cv = None
        
    def print_veriables(self):
        print(self.__dict__)
        

class Job(Data):
    def __init__(self, job_name, ensamble_model_job = False, cv = 10, 
                 n_threads = min(cpu_count()-1, n_threads_max), 
                 problem_type = problem_type, 
                 scoring_function = scoring_function,
                 save_model_tf = False, model_package = None):
        self.job_name = job_name
        self.job_name_full = ''
        self.fl_submission_created = self.job_name
        self.cv = cv
        self.n_threads = n_threads
        self.parameters_best = None
        self.time_start = None
        self.ensamble_model_job = ensamble_model_job
        self.problem_type = problem_type
        self.scoring_function = scoring_function
        self.scoring_function2 = scoring_function2
        self.save_model_tf = save_model_tf
        self.model_package = model_package
        
    def run(self, pipeline, parameters, data):
        result = Result()
        self.time_start = datetime.now()
        model = self.grid_search_and_fit_model(pipeline, parameters, data)
        self.time_model_training_min = (datetime.now() - 
                                        self.time_start).seconds / 60
        self.evaluate_model(data, model, result)
        self.print_and_save_result(data, model, result)
        self.write_submission(data, model, result)
        self.save_model(data, model, result)
        
    def grid_search_and_fit_model(self, pipeline, parameters, data):
        for step in pipeline.steps:
            if step[1].__class__.__name__ == 'ElasticNetCV':
                Warning('Not implemented ElasticNetCV')
        if self.cv == 0:
            cv = ShuffleSplit(len(data.idx_train), test_size=cv_test_size, 
                              n_iter=1, random_state=0)
        else:
            cv = self.cv
        
        if self.model_package != 'keras':
            model = GridSearchCV(pipeline, parameters, cv = cv, 
                                 scoring = scoring_function_scorer,
                                 n_jobs = self.n_threads, verbose = 3)
            model.fit(data.X_train(), data.y_train())
        else:
            fld.create_fld_if_not_exist(fld.get_path(fld.model_scoring,
                                                     self.job_name, 
                                                     'model_ckeckpoint'))
            import datetime
            m_file_name = 'model_ckeckpoint_'+ \
                            datetime.datetime.now().strftime("%Y-%d-%m-%H-%M")\
                             +'-{epoch:04d}-{val_acc:.4f}.hdf5'
            m_file_name = 'model_ckeckpoint.hdf5'
            path = fld.get_path(fld.model_scoring, self.job_name, 
                                'model_ckeckpoint', m_file_name)
            ckpt = ModelCheckpoint(path, monitor='val_loss', verbose=0,
                                   save_best_only=True, save_weights_only=False,
                                   mode='auto')
            es = EarlyStopping(monitor='val_loss', min_delta=0, 
                               patience=10, verbose=0, mode='auto')
            lr_plateau = ReduceLROnPlateau(monitor='val_loss', factor=0.2,
                  patience=5, min_lr=0.000001)
            fld.create_fld_if_not_exist(fld.get_path(fld.model_scoring,
                                                     self.job_name, 
                                                     'RemoteMonitor'))
            path_rm = fld.get_path(fld.model_scoring, self.job_name, 
            'RemoteMonitor')
            rm = RemoteMonitor(path = path_rm)
            try:
                imp.find_module('tensorflow')
                fld.create_fld_if_not_exist(fld.get_path(fld.model_scoring,
                                                     self.job_name, 'TensorBoard'))
                path_tb = fld.get_path(fld.model_scoring, self.job_name, 'TensorBoard')
                tb = TensorBoard(log_dir=path_tb, histogram_freq=0, write_graph=True, 
                                 write_images=False)
                callback_lst = [ckpt, es, lr_plateau, rm, tb]
            except:
                callback_lst = [ckpt, es, lr_plateau, rm]
            model = GridSearchCV(pipeline, parameters, cv = cv,
                                 fit_params = {'m__callbacks': callback_lst},
                                 scoring = scoring_function_scorer,
                                 n_jobs = self.n_threads, verbose = 3)
            # TODO: inlcude cv data split in epoch validation
            # ,'m__validation_data' : (data.X_cv(), data.y_cv())
            model.fit(data.X_train(), data.y_train())
            # TODO: Load best model back and replace one with more ephochs
        return model
    
    def predict(self, model, X, predict_class = True):
        if self.problem_type == 'regression':
            return model.predict(X)
        else:            
            if predict_class:
                return model.predict(X)
            try:
                out = model.predict_proba(X)
            except:
                out = model.predict(X)
            return out
        return None
        
    def evaluate_model(self, data, model, result):
        result.score_train = self.scoring_function(data.y_train(),
                                 self.predict(model, data.X_train()))
        if(len(data.idx_valid) > 0):
            result.score_valid = self.scoring_function(data.y_valid(), 
                                               self.predict(model, data.X_valid()))                    
        result.score_cv = np.float64(float(model.best_score_))
        temp = list()
        for score in model.grid_scores_:
            temp.append(_CVScoreTuple(
                score[0],
                score[1],
                list(score[2])))
                
        result.scores_grid = temp
        result.score2_train = self.scoring_function2(data.y_train(),
                                 self.predict(model, data.X_train()))
        if(len(data.idx_valid) > 0):
            result.score2_valid = self.scoring_function2(data.y_valid(), 
                                               self.predict(model, data.X_valid()))                    
        
        self.parameters_best = model.best_params_
        for step in model.estimator.steps:
            self.job_name_full = str(self.job_name_full) + '_' + step[0]
        return None
    
    def print_and_save_result(self, data, model, result, debug = False, 
                              save_result = True, round_digits = round_digits, 
                              save_classification_report = save_classification_report,
                              fld = fld):
        self.time_start = self.time_start.isoformat()
        self.cv = str(self.cv)
        self.scoring_function = str(self.scoring_function.__name__)
        self.scoring_function2 = str(self.scoring_function2.__name__)
        veriables_all = data.key_veriables()
        veriables_all.update(result.__dict__)
        veriables_all.update(self.__dict__)
        
        if debug:
            print(simplejson.dumps(veriables_all))

        def formated_str(x):
            if isinstance(x, np.float64):
                x = round(x, round_digits)
            return str(x).ljust(round_digits+2)
            
        result_summary = ('# score_LB: ', formated_str('None'), ', ',
                          'score_train: ', formated_str(result.score_train), ', ',
                          'score_cv: ', formated_str(result.score_cv), ', ',
                          'score_valid: ', formated_str(result.score_valid), ', ',
                          'score2_train: ', formated_str(result.score2_train), ', ',
                          'score2_cv: ', formated_str(result.score2_cv), ', ',
                          'score2_valid: ', formated_str(result.score2_valid), ', ',
                          'param_best: ', str(self.parameters_best), ', ',
                          'time_model_training_min: ', 
                          str(round(self.time_model_training_min, 3)))
        print(''.join(result_summary))

        if self.ensamble_model_job:
            fld_save = fld.ensm_result_summary
        else:
            fld_save = fld.model_result_summary

        if save_result:
            with open(fld.get_path(fld_save, 
                                   self.job_name + '.json'), 'a') as f_out:
                simplejson.dump(simplejson.dumps(veriables_all), f_out)
            with open(fld.get_path(fld_save, 
                                   self.job_name + '.txt'), 'a') as f_out:
                f_out.write(''.join(result_summary))
            with open(fld.get_path(fld.logs, 'results.txt'), 'a') as f_out:
                f_out.write(datetime.now().strftime("%Y-%m-%d %H:%M:%S ") + 
                            str(self.job_name) + ' ---------------------\n')
                simplejson.dump(simplejson.dumps(veriables_all), f_out)
                f_out.write('\n')
                f_out.write(''.join(result_summary))            
                f_out.write('\n')
        if save_classification_report:
            from sklearn.metrics import classification_report
            from sklearn.metrics import confusion_matrix
            
            fl_path = fld.get_path(fld_save, self.job_name + '_result.xlsx')
            writer = pd.ExcelWriter(fl_path)

            train_true = data.y_train()
            train_pred = self.predict(model, data.X_train())
            if(len(data.idx_valid) > 0):
                    valid_true = data.y_valid()
                    valid_pred = self.predict(model, data.X_valid())
            
            labels_in_data = data.le.classes_
            le_encoder = pd.DataFrame(labels_in_data)
            le_encoder.to_excel(writer, sheet_name = 'labels')
            
            '''
            for c in np.unique(train_true.extent(train_pred)):
                train_true
               ''' 
            train_cr = classification_report(train_true, train_pred, 
                                             target_names = labels_in_data)
            train_cr = pd.DataFrame({'classification_report': train_cr}, index = [0])
            train_cr.to_excel(writer, sheet_name = 'train_classification_report')
            if(len(data.idx_valid) > 0):
                    valid_cr = classification_report(valid_true, valid_pred, 
                                             target_names = labels_in_data)
                    valid_cr = pd.DataFrame({'classification_report': valid_cr}, 
                                            index = [0])
                    valid_cr.to_excel(writer,
                                      sheet_name = 'valid_classification_report')
            
            train_cm = pd.DataFrame(confusion_matrix(train_true, train_pred))
            train_cm.to_excel(writer, sheet_name = 'train_confusion_matrix')
            if(len(data.idx_valid) > 0):
                valid_cm = pd.DataFrame(confusion_matrix(valid_true, valid_pred))
                valid_cm.to_excel(writer, sheet_name = 'valid_confusion_matrix')
            
            writer.save()
            
        return None
    
    def write_submission(self, data, model, result, fld = fld):
        if self.ensamble_model_job:
            fld_save = fld.ensm_scoring
        else:
            fld_save = fld.model_scoring
        fld.create_fld_if_not_exist(fld.get_path(fld_save, self.job_name))

        if None not in data.fl_submission_input:
            sub = pd.read_csv(fld.get_path(fld.inp_data, *data.fl_submission_input))
        else:
            if data.pd_or_np == 'np':
                raise('Not Implimented')
            else:
                sub = data.df.iloc[data.idx_test][data.fs_id + data.fs_target].reset_index(drop=True)

        output_test = self.predict(model, data.X_test())       
        output_test = [func_target_reverse_transform(x) for x in output_test]
        if data.pd_or_np == 'np':
            out_df = pd.DataFrame(output_test).reset_index(drop = True)
            id_df = pd.DataFrame(data.id[data.idx_test]).reset_index(drop = True)
            id_out = pd.concat([id_df, out_df], axis = 1)
            # id_out.columns = [*data.fs_id] + ['c' + str(i) for i in range(10)]
            test = sub[data.fs_id].reset_index(drop = True)
            test = pd.merge(test, id_out, how = 'left')
        else:
            test = pd.concat([sub[data.fs_id].reset_index(drop = True), 
                      pd.DataFrame(output_test).reset_index(drop = True)], axis = 1,                           
                      ignore_index = True)           
            test.columns = [data.fs_id + data.fs_target]
        
        test.to_csv(fld.get_path(fld_save, self.job_name, 
                            self.fl_submission_created + '_test.csv'),
                   index = False)
                   
        output_train = self.predict(model, data.X_train()) + \
                        self.predict(model, data.X_valid())
        output_train = [func_target_reverse_transform(x) for x in output_train]
        if data.pd_or_np == 'np':
            out_df = pd.DataFrame(output_train).reset_index(drop = True)
            id_df = pd.DataFrame(data.id[data.idx_train]).reset_index(drop = True)
            id_out = pd.concat([id_df, out_df], axis = 1)
            # id_out.columns = [*data.fs_id] + ['c' + str(i) for i in range(10)]
            # TODO: error, check
            train = id_out
        else:
            train = pd.concat([data.df.iloc[data.idx_train][data.fs_id].reset_index(drop = True), 
                          pd.DataFrame(output_train).reset_index(drop = True)], axis = 1,                           
                          ignore_index = True)
            train.columns = [data.fs_id + data.fs_target]
        train = train.sort_values(data.fs_id)
        train.to_csv(fld.get_path(fld_save, self.job_name, 
                            self.fl_submission_created + '_train.csv'),
                   index = False) 
        return None
        
    def save_model(self, data, model, result, fld = fld):
        fld.create_fld_if_not_exist(fld.get_path(fld.model_scoring, 
                                                 self.job_name, 'model'))
        if self.save_model_tf:
            if self.model_package == 'keras':
                path = fld.get_path(fld.model_scoring, self.job_name, 'model', 
                                    'model.h5')
                model.save(path)
            else:
                path = fld.get_path(fld.model_scoring, self.job_name, 'model', 
                                    'model.pkl')
                m = copy.deepcopy(model)
                joblib.dump(m, path)
        return None

    def load_saved_model(self, data, model, result, fld = fld):
        if self.model_package == 'keras':
            path = fld.get_path(fld.model_scoring, self.job_name, 'model', 
                                    'model.h5')
            model = load_model(path)
        else:
            path = fld.get_path(fld.model_scoring, 'ad_truncSVD_randomForest', 'model', 
                                'model.pkl')
            model = joblib.load(path)
        return model
        
# Custom Preprocessors: -------------------------------------------------------
class PDtoNumpy(BaseEstimator, TransformerMixin):
    def __init__(self):
        pass        
        
    def fit(self, X, y=None):
        return self

    def transform(self, X, y=None):
        # out = [x[0].tolist() for x in X]
        # print(len(out))
        # print(out[0].shape)
        # print(out[0])
        out = []
        for x in X:
            out.append(x[0].tolist())
        print(len(out), len(out[0]), len(out[0][0]), len(out[0][0][0]))
        t = np.asarray(out)
        print(t.shape)
        return t

class DataFrameImputer(BaseEstimator, TransformerMixin):
    def __init__(self):
        """Impute missing values.

        Columns of dtype object are imputed with the most frequent value 
        in column.

        Columns of other types are imputed with mean of column.

        """
    def fit(self, X, y=None):

        self.fill = pd.Series([X[c].value_counts().index[0]
            if X[c].dtype == np.dtype('O') else X[c].mean() for c in X],
            index=X.columns)
        print(self.fill)
        return self

    def transform(self, X, y=None):
        return X.fillna(self.fill)

class ColumnExtractor(BaseEstimator, TransformerMixin):
    def __init__(self, idx_num = 'all'):
        self.idx_num = idx_num
        
    def fit(self, X, y=None):
        return self

    def transform(self, X, y = None):
        if self.idx_num == 'all':
            self.idx_num = X.shape[1]
        return X[:,self.idx_num]

class ViewFeatures(BaseEstimator, TransformerMixin):
    def __init__(self, view_features = True):
        self.printed_count = 0
        self.view_features = view_features
    def fit(self, X, y = None):
        return self
        
    def transform(self, X, y = None):
        if (self.view_features) & (self.printed_count == 0):
            print('Feature shape:', X.shape)
            print('Features:')
            print(X)
            self.printed_count = 1
        return X

class ColumnsToText(BaseEstimator, TransformerMixin):
    def __init__(self):
        pass        
        
    def fit(self, m, y=None):
        return self

    def transform(self, m, y=None):
        l = [''] * m.shape[0]
        col = 0
        for c in m.T:
            l = [ x + ' ' + 'col_' + str(col) + '__' + str(y) for x, y in zip(l, c)]
            col += 1
        return l

class Word2VecTransformer(BaseEstimator, TransformerMixin):
    def __init__(self, fl_word_vectors_zip, fl_word_vectors , dim = 300,
                 all_text_data = None):
        self.fl_word_vectors_zip = fl_word_vectors_zip
        self.fl_word_vectors = fl_word_vectors
        self.dim = dim
        self.all_text_data = all_text_data
        
    def fit(self, x, y=None):
        return self

    def transform(self, text, y=None):
        nested_list = [line.split(' ') for line in text]
        word_list = set(list(itertools.chain(*nested_list)))
        word_vectors = self.load_word_vectors(word_list)
        
        features = []
        for line in text:
            feature = [0] * self.dim
            for w in line.split(' '):
                feature = np.add(feature, word_vectors.get(w, [0] * self.dim))
            features.append(feature)
        # print('length features:', len(features))
        # print('length feature:', len(features[0]))
        # print('feature:', features[0])
        return features
    
    def transform_parallel(self, text, y=None):
        # nesting multiprocessing don't work
        nested_list = [line.split(' ') for line in text]
        word_list = set(list(itertools.chain(*nested_list)))
        word_vectors = self.load_word_vectors(word_list)
        
        def get_word_vec(line, word_vectors):
            feature = [0] * self.dim
            for w in line.split(' '):
                feature = np.add(feature, word_vectors.get(w, [0] * self.dim))
            return feature
        
        features = Parallel(n_jobs=n_threads, verbose = 5)(delayed(get_word_vec)(line, 
                            word_vectors) for line in text)         
        # print('length features:', len(features))
        # print('length feature:', len(features[0]))
        # print('feature:', features[0])
        return features

    def load_word_vectors(self, word_list):
        word_vectors = dict()
        with zipfile.ZipFile(fld.get_path(fld.model_meta_data, 
                                          self.fl_word_vectors_zip)) as z:
            with z.open(self.fl_word_vectors) as f:
                counter = 1
                if self.all_text_data is not None:
                    for line in f:
                        splits = line.split()
                        if splits[0] in word_list:
                            word_vectors[splits[0]] = np.array(splits[1:], dtype=np.float)
                        counter += 1
                else:
                    for line in f:
                        splits = line.split()
                        word_vectors[splits[0]] = np.array(splits[1:], dtype=np.float)
                        counter += 1
        return word_vectors        
        
class TextToNumericSequence(BaseEstimator, TransformerMixin):
    def __init__(self, n_max_features = 20000):
        self.n_max_features = n_max_features
        self.tokenizer = Tokenizer(nb_words=n_max_features)
    
    def fit(self, text, y=None):
        self.tokenizer.fit_on_texts(text)
        return self

    def transform(self, text, y=None):
        return self.tokenizer.texts_to_sequences(text)

class PadNumericSequence(BaseEstimator, TransformerMixin):
    def __init__(self, max_seq_len = 80):
        self.max_seq_len = max_seq_len
        
    def fit(self, m, y=None):
        return self

    def transform(self, m, y=None):
        return sequence.pad_sequences(m, maxlen=self.max_seq_len)


# Standard Pipelines: ---------------------------------------------------------
def xx_kbest_logistic(data):
    job = Job('xx_kbest_logistic')
    pipeline = Pipeline(steps=[("univ_select", SelectKBest()),
                               ('logistic', LogisticRegression())])
    parameters = dict(univ_select__k=[15, 25, 35],
                      logistic__C = [1])
    job.run(pipeline, parameters, data)
    return None

def xx_outputCode_logistic(data):
    job = Job('xx_outputCode_logistic')    
    pipeline = Pipeline(steps=[('logistic', OutputCodeClassifier(LogisticRegression()))])
    parameters = dict(logistic__code_size = [0.2])    
    job.run(pipeline, parameters, data)
    return None

def xx_kbest_polinon_kbest_logistic(data):
    job = Job('xx_kbest_polinon_kbest_logistic')
    pipeline = Pipeline(steps=[("univ_select_1", SelectKBest()),
                               ('polinom', PolynomialFeatures()),
                               ("univ_select_2", SelectKBest()),
                               ('logistic', LogisticRegression())])
    parameters = dict(univ_select_1__k=[60],
                      polinom__degree = [2],
                      univ_select_2__k=[120, 240],
                      logistic__C = np.logspace(-1, 1, 3))
    job.run(pipeline, parameters, data)
    return None

def xx_pca_logistic(data):
    job = Job('xx_pca_logistic')
    pipeline = Pipeline(steps=[("pca", PCA()),
                               ('logistic', LogisticRegression())])
    parameters = dict(pca__n_components = [40, 'mle'],
                      logistic__C = np.logspace(-1, 1, 3))
    job.run(pipeline, parameters, data)
    return None
    
def xx_kbest_rbm_logistic(data):
    job = Job('xx_kbest_rbm_logistic')
    pipeline = Pipeline(steps=[("univ_select_1", SelectKBest()),
                               ('rbm', BernoulliRBM()),
                               ('logistic', LogisticRegression())])
    parameters = dict(univ_select_1__k=[40, 60],
                      rbm__n_components = [15, 30],
                      logistic__C = np.logspace(-1, 1, 3))
    job.run(pipeline, parameters, data)
    return None
    
def xx_kbest_polinom_kbest_pca_logistic(data):
    job = Job('xx_kbest_polinom_kbest_pca_logistic')
    pipeline = Pipeline(steps=[("univ_select_1", SelectKBest()),
                               ('polinom', PolynomialFeatures()),
                               ("univ_select_2", SelectKBest()),
                               ('pca', PCA()),
                               ('logistic', LogisticRegression())])
    parameters = dict(univ_select_1__k=[60],
                      polinom__degree = [2],
                      univ_select_2__k=[120],
                      pca__n_components = [40, 'mle'],
                      logistic__C = [0.1, 1])
    job.run(pipeline, parameters, data)
    return None


def xx_fsSelectFromModel_logistic(data):
    job = Job('xx_fsSelectFromModel_logistic')
    pipeline = Pipeline(steps=[('feature_selection', 
                                SelectFromModel(ExtraTreesClassifier())),
                               ('logistic', LogisticRegression())])
    parameters = dict(logistic__C = [0.1, 1, 10])
    job.run(pipeline, parameters, data)
    return None


def xx_kbest_polinom_kbest_randomForest(data):
    job = Job('xx_kbest_polinom_kbest_randomForest')
    pipeline = Pipeline(steps=[("univ_select_1", SelectKBest()),
                               ('polinom', PolynomialFeatures()),
                               ("univ_select_2", SelectKBest()),
                               ('rf', RandomForestClassifier())])
    parameters = dict(univ_select_1__k=[60],
                      polinom__degree = [2],
                      univ_select_2__k=[120],
                      rf__n_estimators = [50, 100, 500])
    job.run(pipeline, parameters, data)                  
    return None


def xx_fsUnion__kbest_polinom_pca_rbm__logistic(data):
    job = Job('xx_fsUnion__kbest_polinom_pca_rbm__logistic')    
    combined_features = FeatureUnion([("univ_select", SelectKBest()),
                                      ('polinom', PolynomialFeatures()),
                                      ("pca", PCA()),
                                      ("rbm", BernoulliRBM())])    
    pipeline = Pipeline(steps=[('features', combined_features),
                               ('logistic', LogisticRegression())])
    parameters = dict(features__univ_select__k=[60],
                      features__polinom__degree = [2],
                      features__pca__n_components=[15, 'mle'],
                      features__rbm__n_components = [15, 50],
                      logistic__C = [0.001, 0.01, 0.1])
    job.run(pipeline, parameters, data)
    return None

def xx_kbest_polinom_kbest_elasticNet(data):
    job = Job('xx_kbest_polinom_kbest_elasticNet')
    pipeline = Pipeline(steps=[("univ_select_1", SelectKBest()),
                               ('polinom', PolynomialFeatures()),
                               ("univ_select_2", SelectKBest()),
                               ('en', ElasticNet(max_iter = 3000))])
    parameters = dict(univ_select_1__k=[60],
                      polinom__degree = [2],
                      univ_select_2__k=[250, 1000],
                      en__l1_ratio = [0.1])
    job.run(pipeline, parameters, data)

def xx_XGBoost(data):
    job = Job('xx_XGBoost')
    pipeline = Pipeline(steps=[('xgb', XGBClassifier())])
    parameters = dict(xgb__n_estimators = [400, 560, 650],
                      xgb__objective = ["binary:logistic"], 
                      xgb__learning_rate = [0.01, 0.0202048],
                      xgb__max_depth = [5, 7, 10],
                      xgb__subsample = [0.6815, 0.8],
                      xgb__colsample_bytree = [0.701, 0.8])
    job.run(pipeline, parameters, data)
    return None
    
def xx_fsUnion__polinom_pca_rbm__XGBoost(data):
    job = Job('xx_fsUnion__polinom_pca_rbm__XGBoost')    
    combined_features = FeatureUnion([("univ_select", SelectKBest()),
                                      ('polinom', PolynomialFeatures()),
                                      ("pca", PCA()),
                                      ("rbm", BernoulliRBM())])    
    pipeline = Pipeline(steps=[('features', combined_features),
                               ('xgb', XGBClassifier())])
    parameters = dict(features__univ_select__k=['all'],
                      features__polinom__degree = [2],
                      features__pca__n_components=[18],
                      features__rbm__n_components = [15])
    job.run(pipeline, parameters, data)
    return None

def xx_vecAvg_multinomial(data):
    job = Job('ah_vecAvg_multinomial')
    pipeline = Pipeline(steps=[("vecAvg", Word2VecTransformer(fld.get_path(fld.model_meta_data, fl_word_vectors), 
                                                              dim = 300,
                                                              all_text_data = list(data.df.text))),
                               ('logistic', LogisticRegression())])
    parameters = dict(logistic__C = [1])
    job.run(pipeline, parameters, data)
    return None

def xx_vecAvg_randomForest(data):
    job = Job('xx_vecAvg_randomForest')
    pipeline = Pipeline(steps=[("vecAvg", Word2VecTransformer(fld.get_path(fld.model_meta_data, fl_word_vectors), 
                                                              dim = 300,
                                                              all_text_data = list(data.df.text))),
                               ('rf', RandomForestClassifier())])
    parameters = dict(rf__n_estimators = [30, 90, 270])
    job.run(pipeline, parameters, data)
    return None    

def xx_text_tfidf_elaticnet(data):
    job = Job('xx_text_tfidf_elaticnet', cv = 0)
    pipeline = Pipeline(steps=[('ColToText', ColumnsToText()),
                               ("tfidf", TfidfVectorizer(stop_words = 'english')),
                               ('reg', SGDRegressor())])
    parameters = dict(tfidf__norm = ['l2'],
                      tfidf__ngram_range = [(1, 1)],
                      reg__penalty = ['elasticnet'],
                      reg__alpha = [1e-4], # [1e-4, 1e-3, 1e-2, 1e-1]
                      reg__l1_ratio = [0.15]) # [0.1, 0.5, 0.8, 0.9, 0.99]
    job.run(pipeline, parameters, data)
    return None

def xx_text_truncSVD_randomForest(data):
    job = Job('xx_text_truncSVD_randomForest', cv = 0)
    pipeline = Pipeline(steps=[('ColToText', ColumnsToText()),
                               ("tfidf", TfidfVectorizer(stop_words = 'english')),
                               ("trunc_svd", TruncatedSVD()),
                               ('reg', RandomForestRegressor())])
    parameters = dict(tfidf__norm = ['l2'],
                      tfidf__ngram_range = [(1, 1)],
                      trunc_svd__n_components = [9],
                      reg__n_estimators = [27])
    job.run(pipeline, parameters, data)
    return None

def ag_text_tfidf_XGBoost(data):
    job = Job('ag_text_tfidf_XGBoost', cv = 0, n_threads = 1)
    pipeline = Pipeline(steps=[('ColToText', ColumnsToText()),
                               ("tfidf", TfidfVectorizer(stop_words = 'english')),
                               ('xgb', XGBClassifier(silent = False))])
    parameters = dict(tfidf__norm = ['l2'],
                      tfidf__ngram_range = [(1, 1)],
                      xgb__n_estimators = [100],
                      xgb__objective = ['binary:logistic'], 
                      xgb__learning_rate = [0.1],
                      xgb__max_depth = [3, 9, 27],
                      xgb__subsample = [1],
                      xgb__colsample_bytree = [1])
    job.run(pipeline, parameters, data)
    return None

def ah_fsUnion__oneHot_trunkSVD_num__regression(data):
    job = Job('ah_fsUnion__oneHot_trunkSVD_num__regression', cv = 0)
    combined_features = FeatureUnion([('cat_one_hot', 
                                        Pipeline(steps = [('fs_one_hot', 
                                                           ColumnExtractor(data.fs_index(data.fs_categorical))),
                                                  ('one_hot', 
                                                   OneHotEncoder(handle_unknown = 'ignore')),
                                                  ("trunc_svd", 
                                                   TruncatedSVD(n_components = 100))
                                        ])),
                                      ('num_scale', 
                                        Pipeline(steps = [('numeric', 
                                                           ColumnExtractor(data.fs_index(data.fs_numeric))),
                                                  ('scale', Normalizer())
                                        ]))
                                     ])
    pipeline = Pipeline(steps=[('com_fea', combined_features),
                               ('view_features', ViewFeatures()),
                               ('reg', SGDClassifier())])
    parameters = dict(reg__penalty = ['l2']) 
    job.run(pipeline, parameters, data)
    return None



def cnn(hidden_dims=100, max_features = 20000, embedding_dims = 128,
                 max_seq_len = 50, nb_filter = 50, filter_length = 5):
    model = Sequential()
    model.add(Embedding(max_features,
                        embedding_dims,
                        input_length=max_seq_len,
                        dropout=0.2))
    model.add(Convolution1D(nb_filter=nb_filter,
                            filter_length=filter_length,
                            border_mode='valid',
                            activation='relu',
                            subsample_length=1))
    def max_1d(X):
        return K.max(X, axis=1)

    model.add(Lambda(max_1d, output_shape=(nb_filter,)))
    model.add(Dense(hidden_dims))
    model.add(Dropout(0.5))
    model.add(Activation('relu'))
    model.add(Dense(1))
    model.add(Activation('sigmoid'))
    model.compile(loss='binary_crossentropy',
                  optimizer='adam',
                  metrics=['accuracy'])
    return model
        
def xx_embedding_cnn(data): 
    job = Job('aj_embedding_cnn', cv = 0, n_threads = 1)
    cnn_clf = KerasClassifier(build_fn=cnn, batch_size=32, nb_epoch=2)
    pipeline = Pipeline(steps=[('txt_to_seq', TextToNumericSequence()),
                               ('padd_seq', PadNumericSequence()),
                                ('cnn', cnn_clf)])
    parameters = dict(padd_seq__max_seq_len = [50],
                      cnn__max_seq_len = [50],
                      cnn__hidden_dims = [50])
    job.run(pipeline, parameters, data)
    return None

def lstm(max_features = 20000, embedding_dims = 128, max_seq_len = 100):
    model = Sequential()
    model.add(Embedding(max_features, embedding_dims, input_length=max_seq_len, 
                        dropout=0.2))
    model.add(LSTM(embedding_dims, dropout_W=0.2, dropout_U=0.2))  
    model.add(Dense(1))
    model.add(Activation('sigmoid'))
    model.compile(loss='binary_crossentropy',
                  optimizer='adam',
                  metrics=['accuracy'])        
    return model
        
def xx_embedding_lstm(data):
    job = Job('ak_embedding_lstm', cv = 0, n_threads = 1)
    lstm_clf = KerasClassifier(build_fn=lstm, batch_size=32, nb_epoch=2)

    pipeline = Pipeline(steps=[('txt_to_seq', TextToNumericSequence()),
                               ('padd_seq', PadNumericSequence()),
                               ('lstm', lstm_clf)])
    parameters = dict(padd_seq__max_seq_len = [50],
                      lstm__max_seq_len = [50],
                      lstm__embedding_dims = [128],
                      lstm__nb_epoch = [3])
    job.run(pipeline, parameters, data)
    return None
    
    
def zz_feature_union(data):
    from sklearn.base import BaseEstimator, TransformerMixin
    from sklearn.datasets.twenty_newsgroups import strip_newsgroup_footer
    from sklearn.datasets.twenty_newsgroups import strip_newsgroup_quoting
    class ItemSelector(BaseEstimator, TransformerMixin):
        """For data grouped by feature, select subset of data at a provided key.
    
        The data is expected to be stored in a 2D data structure, where the first
        index is over features and the second is over samples.  i.e.
    
        >> len(data[key]) == n_samples
    
        Please note that this is the opposite convention to sklearn feature
        matrixes (where the first index corresponds to sample).
    
        ItemSelector only requires that the collection implement getitem
        (data[key]).  Examples include: a dict of lists, 2D numpy array, Pandas
        DataFrame, numpy record array, etc.
    
        >> data = {'a': [1, 5, 2, 5, 2, 8],
                   'b': [9, 4, 1, 4, 1, 3]}
        >> ds = ItemSelector(key='a')
        >> data['a'] == ds.transform(data)
    
        ItemSelector is not designed to handle data grouped by sample.  (e.g. a
        list of dicts).  If your data is structured this way, consider a
        transformer along the lines of `sklearn.feature_extraction.DictVectorizer`.
    
        Parameters
        ----------
        key : hashable, required
            The key corresponding to the desired value in a mappable.
        """
        def __init__(self, key):
            self.key = key
    
        def fit(self, x, y=None):
            return self
    
        def transform(self, data_dict):
            return data_dict[self.key]
    
    
    class TextStats(BaseEstimator, TransformerMixin):
        """Extract features from each document for DictVectorizer"""
    
        def fit(self, x, y=None):
            return self
    
        def transform(self, posts):
            return [{'length': len(text),
                     'num_sentences': text.count('.')}
                    for text in posts]
    
    
    class SubjectBodyExtractor(BaseEstimator, TransformerMixin):
        """Extract the subject & body from a usenet post in a single pass.
    
        Takes a sequence of strings and produces a dict of sequences.  Keys are
        `subject` and `body`.
        """
        def fit(self, x, y=None):
            return self
    
        def transform(self, posts):
            features = np.recarray(shape=(len(posts),),
                                   dtype=[('subject', object), ('body', object)])
            for i, text in enumerate(posts):
                headers, _, bod = text.partition('\n\n')
                bod = strip_newsgroup_footer(bod)
                bod = strip_newsgroup_quoting(bod)
                features['body'][i] = bod
    
                prefix = 'Subject:'
                sub = ''
                for line in headers.split('\n'):
                    if line.startswith(prefix):
                        sub = line[len(prefix):]
                        break
                features['subject'][i] = sub
    
            return features
    

    pipeline = Pipeline([
    # Extract the subject & body
    ('subjectbody', SubjectBodyExtractor()),

    # Use FeatureUnion to combine the features from subject and body
    ('union', FeatureUnion(
        transformer_list=[

            # Pipeline for pulling features from the post's subject line
            ('subject', Pipeline([
                ('selector', ItemSelector(key='subject')),
                ('tfidf', TfidfVectorizer(min_df=50)),
            ])),

            # Pipeline for standard bag-of-words model for body
            ('body_bow', Pipeline([
                ('selector', ItemSelector(key='body')),
                ('tfidf', TfidfVectorizer()),
                ('best', TruncatedSVD(n_components=50)),
            ])),

            # Pipeline for pulling ad hoc features from post's body
            ('body_stats', Pipeline([
                ('selector', ItemSelector(key='body')),
                ('stats', TextStats()),  # returns a list of dicts
                ('vect', DictVectorizer()),  # list of dicts -> feature matrix
            ])),

        ],

        # weight components in FeatureUnion
        transformer_weights={
            'subject': 0.8,
            'body_bow': 0.5,
            'body_stats': 1.0,
        },
    )),

    # Use a SVC classifier on the combined features
    ('svc', SVC(kernel='linear')),
    ])
    return pipeline


# Functions: ------------------------------------------------------------------



# Run Code: -------------------------------------------------------------------
def main():
    pass

if __name__ == "__main__":
    main()
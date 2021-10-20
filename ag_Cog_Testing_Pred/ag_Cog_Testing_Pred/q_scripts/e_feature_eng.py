# -*- coding: utf-8 -*-
"""
@author: nilesh

Tasks: Completed
Tasks: Pending
"""

from __future__ import print_function
import os
import sys
import timeit
import pandas as pd
import numpy as np
import pickle
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.decomposition import TruncatedSVD
from itertools import combinations
from imblearn.combine import SMOTEENN


# User Inputs: ----------------------------------------------------------------
path_base = os.path.dirname(os.path.dirname(os.path.realpath(__file__))) 
fl_inp_train = ''
fl_inp_test = ''
fl_inp_pp_train = 'aa_TL_DRM_IDA_Traget.csv.gz'
fl_inp_pp_test = 'aa_TL_DRM_IDA_Traget.csv.gz'
fl_feature_eng = 'aa_tfidf_truncSVD.pkl'
fl_sub_input = ''

# Set Base Path: --------------------------------------------------------------
os.chdir(path_base)
sys.path.append(path_base)
from q_scripts.a_class_func_dir import DirStructure, Data


# Read Config File: -----------------------------------------------------------
fld = DirStructure('config.ini')

def get_tfidf_truncSVD_features(data, fs_text = 'text', ngram_range = (1, 1),
                                min_df = 5, n_components = 1000, verbose = 0):
    if verbose > 0: print('TFIDF transformation started')
    text_train_cv = data.df[fs_text].values[data.idx_train]
    tfidf_model = TfidfVectorizer(stop_words = 'english', 
                            ngram_range = ngram_range,
                            min_df = min_df).fit(text_train_cv)
    tfidf_train = tfidf_model.transform(text_train_cv)
    if verbose > 0: print('TFIDF transformation complete')
    tsvd_model = TruncatedSVD(n_components = n_components).fit(tfidf_train)
    if verbose > 0: print('tSVD model fit complete')
    print('Explained variance:', np.sum(tsvd_model.explained_variance_ratio_))
    # Explained variance: 0.6406
    
    '''
    with open(fld.get_path(fld.data_featur_eng, 'tfidf_model.pkl'), 'wb') as fl:
            pickle.dump(tfidf_model, fl, pickle.HIGHEST_PROTOCOL)
    
    with open(fld.get_path(fld.data_featur_eng, 'tsvd_model.pkl'), 'wb') as fl:
            pickle.dump(tsvd_model, fl, pickle.HIGHEST_PROTOCOL)
    '''
    print('tfidf_train:', tfidf_train)
    print('tsvd_model:', tsvd_model)
    tfidf = tfidf_model.transform(data.df[fs_text].values)
    tsvd = tsvd_model.transform(tfidf)
    if verbose > 0: print('tSVD transformation complete')
    tsvd = pd.DataFrame(tsvd)
    fe_columns = ['tfidf_tSVD_' + str(i) for i in range(n_components)]
    tsvd.columns = fe_columns

    data.df = data.df.join(tsvd)
    data.fs_ind = fe_columns
    return data

def create_and_save_features(fl_inp_pp_train, fl_inp_pp_test, fl_feature_eng,
                              n_components = 2000):
    data = Data(pd_or_np = 'pd', fl_submission_input = fl_sub_input)
    data.load_pre_processed(fl_inp_pp_train, fl_inp_pp_test,
                            nrows_train = None, nrows_test = 0)
    data.set_idx_train_valid_test(0)
    data.set_fs(fs_target = ['label'], fs_id = ['id'], fs_ind = 'text')
    data = get_tfidf_truncSVD_features(data, fs_text = 'text', ngram_range = (1, 2),
                                n_components = n_components, verbose=1)

    # may not working due to python issue for large files 2+ gb
    data.save(fld.data_featur_eng, fl_feature_eng)
    return None

def load_features(fl_feature_eng):
    d = Data()
    d.load(fld.data_featur_eng, fl_feature_eng)
    tsvd = d.X_train()
    y = d.y_train()
    return tsvd, y

def load_tSVD_balanced_data():
    with open(fld.get_path(fld.data_featur_eng, 'balanced_datasets.pkl'), 'rb') as f:
        balanced_data = pickle.load(f)
    return balanced_data

def create_balanced_pairs_data(input_data,input_labels,balance='True',
                     threshold_to_ignore=0.8,balance_ratio=0.3):
    dict_of_datasets={}
    list_of_combinations=[]
    for each_combo in combinations(np.unique(input_labels),2):
        list_of_combinations.append(each_combo)
    for each_combo in list_of_combinations:
        dict_of_datasets[each_combo]={}
        val0=each_combo[0]
        val1=each_combo[1]
        this_x=[]
        this_y=[]
        count_val0=np.sum(input_labels==val0)
        count_val1=np.sum(input_labels==val1)
        this_x=np.vstack([np.array(input_data[input_labels==val0,]),
                          np.array(input_data[input_labels==val1,])])
        this_y=np.concatenate([np.array(input_labels[input_labels==val0]),
                               np.array(input_labels[input_labels==val1])])
        if count_val0<count_val1:
            ratio=float(count_val0)/count_val1
        else:
            ratio=float(count_val1)/count_val0
        if balance==True:
            if ratio<threshold_to_ignore:
                sme = SMOTEENN(ratio=balance_ratio,random_state=123)
                X_res, y_res = sme.fit_sample(this_x,this_y)
                dict_of_datasets[each_combo]['x']=X_res
                dict_of_datasets[each_combo]['y']=y_res
                dict_of_datasets[each_combo]['rebalanced']=True
            else:
                dict_of_datasets[each_combo]['x']=this_x
                dict_of_datasets[each_combo]['y']=this_y
                dict_of_datasets[each_combo]['rebalanced']=False
      
    with open(fld.get_path(fld.data_featur_eng, 'balanced_pairs_datasets.pkl'), 'wb') as f:
        pickle.dump(f, dict_of_datasets)
    return dict_of_datasets

def create_and_save_features_pairs_balanced(fl_inp_pp_train, fl_inp_pp_test,
                                            fl_feature_eng,
                              n_components = 2000):
    data = Data(pd_or_np = 'pd', fl_submission_input = fl_sub_input)
    data.load_pre_processed(fl_inp_pp_train, fl_inp_pp_test,
                            nrows_train = None, nrows_test = 0)
    data.set_idx_train_valid_test(0.2)
    data.set_fs(fs_target = ['label'], fs_id = ['id'], fs_ind = 'text')
    data = get_tfidf_truncSVD_features(data, fs_text = 'text', ngram_range = (1, 2),
                                n_components = n_components, verbose=1)
    
    # may not working due to python issue for large files 2+ gb
    data.save(fld.data_featur_eng, fl_feature_eng)
    return None
    
# Run Code: -------------------------------------------------------------------
def main():
    create_and_save_features(fl_inp_pp_train, fl_inp_pp_test, fl_feature_eng,
                              n_components = 2000)
    tsvd, y = load_features(fl_feature_eng)
    print(tsvd)
    print(y)
    print(len(tsvd))
    print(len(y))
    
if __name__ == "__main__":
    start_time = timeit.default_timer()
    main()
    print('Time taken:', timeit.default_timer() - start_time)

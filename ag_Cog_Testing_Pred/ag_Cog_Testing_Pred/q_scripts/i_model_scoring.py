# -*- coding: utf-8 -*-
"""
@author: nilesh

Tasks: Completed --------------------------------------------------------------

Tasks: Pending ----------------------------------------------------------------

Refrences: --------------------------------------------------------------------


Refrences Data: ---------------------------------------------------------------

External Repos: ---------------------------------------------------------------

Issues: -----------------------------------------------------------------------
1. How to save Scikit-Learn-Keras Model into a Persistence File https://github.com/fchollet/keras/issues/4274


"""
from __future__ import print_function
import sys
import os
import timeit
import copy
import numpy as np
import mkl
import platform
import pandas as pd
import datetime
from sklearn.externals import joblib
import imp

from sklearn.preprocessing import LabelEncoder
from sklearn.decomposition import TruncatedSVD
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.pipeline import Pipeline
from sklearn.linear_model import SGDClassifier, LogisticRegression
from sklearn.linear_model import BayesianRidge
from sklearn.ensemble import RandomForestClassifier, GradientBoostingClassifier
from sklearn.ensemble import ExtraTreesRegressor, AdaBoostRegressor
from sklearn.multiclass import OutputCodeClassifier
# from xgboost.sklearn import XGBRegressor

from keras.models import Model
from keras.models import Sequential
from keras.layers.core import Dense, Activation
from keras.wrappers.scikit_learn import KerasClassifier
from keras.layers import Dropout, Embedding, LSTM, MaxPooling1D, Flatten, Merge
from keras.layers import Convolution1D, AveragePooling1D, Conv1D, Input, GlobalAveragePooling1D
from keras.preprocessing.text import Tokenizer
from keras.optimizers import Nadam, RMSprop
from keras.utils.np_utils import to_categorical
from keras.models import load_model
from keras.callbacks import ModelCheckpoint, EarlyStopping, TensorBoard
from keras.callbacks import RemoteMonitor, ReduceLROnPlateau


# User Inputs -----------------------------------------------------------------
path_base = os.path.dirname(os.path.dirname(os.path.realpath(__file__))) 
if platform.system() == 'Linux':
    mkl.set_num_threads(8)
else:
    mkl.set_num_threads(4)

fl_inp_pp_train = 'aa_CI_DRM_IDC_Target_noTP.csv.gz'
fl_inp_pp_test = 'aa_CI_DRM_IDC_Target_noTP.csv.gz'


fl_word_vectors_zip = 'ab_glove.6B.300d.zip'
fl_word_vectors = 'ab_glove.6B.300d.txt'

n_output_hidden_units = 37
cv_n_fold = 10
cv_n_fold_dl = 0

# Set Base Path----------------------------------------------------------------
os.chdir(path_base)
sys.path.append(path_base)
from q_scripts.a_class_func_dir import DirStructure, Job, Data
from q_scripts.a_class_func_dir import TextToNumericSequence, PadNumericSequence
from q_scripts.a_class_func_dir import scoring_function

# Read Config File: -----------------------------------------------------------
fld = DirStructure('config.ini')

def pre_process_observation(obs):
    # TODO: (Anil) add preprocessing script i.e. json --> preprocessing --> input for model
    # TODO: (Anil) add error handelling, send class Non_Classifiable if both title and description is blank
    # TODO: (Anil) tell client that Non_Classifiable will be present in case of error
    # TODO: (Anil) specify input format to client

    return obs

def create_embedding_matrix(texts, max_features=20000, embedding_dims = 100):
    embeddings_index = {}
    import zipfile
    with zipfile.ZipFile(fld.get_path(fld.model_meta_data, fl_word_vectors_zip)) as z:
        with z.open(fl_word_vectors) as f:
            for line in f:
                values = line.split()
                word = values[0]
                coefs = np.asarray(values[1:], dtype='float32')
                embeddings_index[word] = coefs

    tokenizer = Tokenizer(nb_words=max_features)
    tokenizer.fit_on_texts(texts)
    word_index = tokenizer.word_index
    nb_words = min(max_features, len(word_index))
    embedding_matrix = np.zeros((nb_words + 1, embedding_dims))
    for word, i in word_index.items():
        if i > max_features:
            continue
        embedding_vector = embeddings_index.get(word)
        if embedding_vector is not None:
            embedding_matrix[i] = embedding_vector
    return embedding_matrix
    
def glove_fasttext(max_features = 20000, embedding_dims = 300, max_seq_len = 300,
                   nb_filter = 64):
    embedding_layer = Embedding(max_features + 1,
                                embedding_dims,
                                weights=[embedding_matrix],
                                input_length=max_seq_len,
                                trainable=True)
    model = Sequential()
    model.add(embedding_layer)
    model.add(GlobalAveragePooling1D())
    
    model.add(Dense(n_output_hidden_units))
    model.add(Activation('softmax'))
    optm = Nadam(lr=0.0001, schedule_decay=0.004)
    
    # optm = RMSprop(lr=0.001)
    model.compile(loss='categorical_crossentropy',
                  optimizer=optm, metrics = ['accuracy'])
    return model
    
def get_callbacks(job):
    fld.create_fld_if_not_exist(fld.get_path(fld.model_scoring,
                                                     job.job_name, 
                                                     'model_ckeckpoint'))
    m_file_name = 'model_ckeckpoint_'+ \
                    datetime.datetime.now().strftime("%Y-%d-%m-%H-%M")\
                     +'-{epoch:04d}-{val_acc:.4f}.hdf5'
    m_file_name = 'model_ckeckpoint.hdf5'
    path = fld.get_path(fld.model_scoring, job.job_name, 
                        'model_ckeckpoint', m_file_name)
    ckpt = ModelCheckpoint(path, monitor='val_loss', verbose=0,
                           save_best_only=True, save_weights_only=False,
                           mode='auto')
    es = EarlyStopping(monitor='val_loss', min_delta=0, 
                       patience=10, verbose=0, mode='auto')
    lr_plateau = ReduceLROnPlateau(monitor='val_loss', factor=0.2,
          patience=5, min_lr=0.000001)
    fld.create_fld_if_not_exist(fld.get_path(fld.model_scoring,
                                             job.job_name, 
                                             'RemoteMonitor'))
    path_rm = fld.get_path(fld.model_scoring, job.job_name, 
    'RemoteMonitor')
    rm = RemoteMonitor(path = path_rm)
    try:
        imp.find_module('tensorflow')
        fld.create_fld_if_not_exist(fld.get_path(fld.model_scoring,
                                             job.job_name, 'TensorBoard'))
        path_tb = fld.get_path(fld.model_scoring, job.job_name, 'TensorBoard')
        tb = TensorBoard(log_dir=path_tb, histogram_freq=0, write_graph=True, 
                         write_images=False)
        callback_lst = [ckpt, es, lr_plateau, rm, tb]
    except:
        callback_lst = [ckpt, es, lr_plateau, rm]
    return callback_lst
    
def am_glove_fasttext_CI_DRM_IDC_Target_noTP(data, train_or_load = 'train'):
    job = Job('CI_DRM_IDC_Target_noTP', cv = cv_n_fold_dl, n_threads = 1,
              save_model_tf = True, model_package = None)
    if train_or_load == 'train':
        try:
            max_features = 25000
            max_seq_len = 400
            embedding_dims = 300
            batch_size = 64
            nb_epoch = 200
            
            def pre_process_data_for_deep_learning(data, fs_text = 'text', verbose = 0):
                pipeline = Pipeline(steps=[('txt_to_seq', TextToNumericSequence(max_features)),
                                           ('padd_seq', PadNumericSequence(max_seq_len))])
                m_pre_proc = pipeline.fit(data.df[fs_text].values)
                text_num = m_pre_proc.transform(data.df[fs_text].values)
                # text_num = np.array(text_num)
                return text_num, m_pre_proc
            text_num, m_pre_proc = pre_process_data_for_deep_learning(data, 
                                                                     fs_text = data.fs_ind)
            
            global embedding_matrix
            embedding_matrix = create_embedding_matrix(data.df[data.fs_ind], 
                                                       max_features=max_features,
                                                       embedding_dims = embedding_dims)
            model = glove_fasttext(max_features = max_features, 
                                   embedding_dims = embedding_dims, 
                                   max_seq_len = max_seq_len)
            callback_lst = get_callbacks(job)
            if len(data.idx_valid) != 0:
                model.fit(text_num[data.idx_train], to_categorical(data.y_train(), 
                                                      nb_classes = n_output_hidden_units),
                          validation_data = (text_num[data.idx_valid], 
                                             to_categorical(data.y_valid(), 
                                               nb_classes = n_output_hidden_units)),
                          batch_size = batch_size, nb_epoch = nb_epoch,  verbose = 1,
                          callbacks = callback_lst)
                print('CV score:', scoring_function(data.y_valid(),
                                     model.predict_classes(text_num[data.idx_valid])))
            else:
               model.fit(text_num[data.idx_train], to_categorical(data.y_train(), 
                          nb_classes = n_output_hidden_units),
                          validation_split = 0.05,
                          batch_size = batch_size, nb_epoch = nb_epoch,  verbose = 1,
                          callbacks = callback_lst)
            
            fld.create_fld_if_not_exist(fld.get_path(fld.model_scoring, 
                                                     job.job_name, 'model'))
            path = fld.get_path(fld.model_scoring, job.job_name, 'model', 
                                        'model.h5')
            model.save(path)
            path = fld.get_path(fld.model_scoring, job.job_name, 'model', 
                                        'model_pre_proc.pkl')
            joblib.dump(m_pre_proc, path)
            print('Model saved')
        except Exception as e:
                error_code = 'model_training_failed'
                error_log = (str(e))
                print(error_code)
                print(error_log)
    elif train_or_load == 'load':
        path = fld.get_path(fld.model_scoring, job.job_name, 'model', 
                                    'model_pre_proc.pkl')
        m_pre_proc = joblib.load(path)
        path = fld.get_path(fld.model_scoring, job.job_name, 'model',
                                    'model.h5')
        model = load_model(path)
        return m_pre_proc, model
    else:
        raise('train_or_load should be either "train" or "load"')
    return None
    
def post_process_observation(out):
    # TODO: (Anil) assign labels to integer output from model i.e. script to 
    # replicate le.inverse_transform
    return out
    
def predict_class(obs, model):
    obs = pre_process_observation(obs)
    out = model.predict(obs)
    out = post_process_observation(out)
    return out

def predict_proba(obs, model):
    obs = pre_process_observation(obs)
    out = model.predict_proba(obs)
    out = post_process_observation(out)
    return out   
    
def predict_proba_keras(obs, model, m_pre_proc):
    obs = pre_process_observation(obs)
    obs = m_pre_proc.transform(obs)
    out = model.predict_proba(obs)
    out = post_process_observation(out)
    return out   
    
def unit_test_predict_class(data, model):
    data.load_pre_processed(fl_inp_pp_train, fl_inp_pp_test,
                                nrows_train = None, nrows_test = 100)
    data.set_idx_train_valid_test(valid_as_pct_of_train = 0)
    data.set_fs(fs_target = ['label'], fs_id = ['id'],
                fs_ind = 'text')
    n = np.random.random_integers(100)
    obs = [data.df.ix[n, 'text']]
    print('------------------------------------------------------------------')    
    print('Warning: Please manualy verify output as predicted value may not match true value')    
    print('Observation:\n', obs)
    print('------------------------------------------------------------------')    
    out = predict_proba(obs, model)
    print('------------------------------------------------------------------')    
    print('Output Predicted:\n', out)
    print('------------------------------------------------------------------')    
    print('Output True:\n', data.df.ix[n])
    print('------------------------------------------------------------------')    
    return None

def unit_test_predict_proba(data, model):
    data.load_pre_processed(fl_inp_pp_train, fl_inp_pp_test,
                                nrows_train = None, nrows_test = 100)
    data.set_idx_train_valid_test(valid_as_pct_of_train = 0)
    data.set_fs(fs_target = ['label'], fs_id = ['id'],
                fs_ind = 'text')
    n = np.random.random_integers(100)
    obs = [data.df.ix[n, 'text']]
    print('------------------------------------------------------------------')    
    print('Warning: Please manualy verify output as predicted value may not match true value')    
    print('Observation:\n', obs)
    print('------------------------------------------------------------------')    
    out = predict_proba(obs, model)
    print('------------------------------------------------------------------')    
    print('Output Predicted:\n', out)
    print('------------------------------------------------------------------')    
    print('Output True:\n', data.df.ix[n])
    print('------------------------------------------------------------------')    
    return None

def unit_test_predict_proba_keras(data, model, m_pre_proc):
    data.load_pre_processed(fl_inp_pp_train, fl_inp_pp_test,
                                nrows_train = None, nrows_test = 100)
    data.set_idx_train_valid_test(valid_as_pct_of_train = 0)
    data.set_fs(fs_target = ['label'], fs_id = ['id'],
                fs_ind = 'text')
    n = np.random.random_integers(100)
    obs = [data.df.ix[n, 'text']]
    print('------------------------------------------------------------------')    
    print('Warning: Please manualy verify output as predicted value may not match true value')    
    print('Observation:\n', obs)
    print('------------------------------------------------------------------')    
    out = predict_proba_keras(obs, model, m_pre_proc)
    print('------------------------------------------------------------------')    
    print('Output Predicted:\n', out)
    print('------------------------------------------------------------------')    
    print('Output True:\n', data.df.ix[n])
    print('------------------------------------------------------------------')    
    return None

def unit_test_predict_proba_multiple_obs(data, model):
    data.load_pre_processed(fl_inp_pp_train, fl_inp_pp_test,
                                nrows_train = None, nrows_test = 100)
    data.set_idx_train_valid_test(valid_as_pct_of_train = 0)
    data.set_fs(fs_target = ['label'], fs_id = ['id'],
                fs_ind = 'text')
    ns = [1, 6, 16, 61]
    obs = data.df.ix[ns, 'text'].values
    class_true = data.df.ix[ns, 'label'].values
    out = predict_proba(obs, model)
    df = pd.DataFrame(out)
    df['text'] = obs
    df['class_true'] = class_true
    cols = ['build package','code','data','database','design','environment','national language support','requirements','test process','user documentation']
    cols = cols  + ['text', 'class_true'] 
    df.columns = cols
    df.to_csv(fld.get_path(fld.model_verification, 'probabality_prediction.csv'))
    return None
    
# Run Code: -------------------------------------------------------------------
def main():
    data = Data(pd_or_np = 'pd', fl_submission_input = None)
    
    if True:
        ### Model Training ###
        data.load_pre_processed(fl_inp_pp_train, fl_inp_pp_test,
                                nrows_train = None, nrows_test = 100)
        data.set_idx_train_valid_test(valid_as_pct_of_train = 0)
        data.set_fs(fs_target = ['label'], fs_id = ['id'],
                    fs_ind = 'text')
        x = data.df[data.fs_target].values.ravel()
        print('Target distribution:\n', data.df.label.value_counts())
        le = LabelEncoder().fit(x)
        data.df[data.fs_target] = le.transform(x)
        data.le =  le
        data.describe()
        print('Class lables in order:')
        for c in le.classes_: 
            print(c)
        # TODO: Anil to use below sequence to assign probabality to respective labels
        # ["accuracy", "clarity", "completeness", "complex path", "configuration compliance", "consistency", "correctness", "data migration", "database performance", "database structure", "design conformance", "feasibility", "happy path", "hardware configuration", "interaction", "internal document", "language dependency", "lateral compatibility", "logic flow", "navigation", "organization", "prerequisite corequisite", "rare situation", "readability", "recovery path", "retrievability", "screen text characters", "sequencing", "simple path", "software configuration", "startup restart", "task orientation", "testability", "variation", "widget icon appearance", "widget icon behavior", "workload stress"]
        print('Building Model')
        am_glove_fasttext_CI_DRM_IDC_Target_noTP(data, train_or_load = 'train')
        # With validation set of 5%
        # loss: 0.7414 - acc: 0.7542 - val_loss: 1.0085 - val_acc: 0.6880
    else:
        ### Scoring ###
        try:
            m_pre_proc, model = am_glove_fasttext_CI_DRM_IDC_Target_noTP(data, train_or_load = 'load')
            # TODO: (Nilesh) [Done] add error handelling if model does not load and predict function return error
            # TODO: (ANIL) load 'obs'
            try:
                obs = ['brs21 selltosave cr11 contract selected default added wl hpp complete new path retention usergroup steps reproduce 1 initiate order new path retention usergroup ict retention move next page 2 customer information ']
                out = predict_proba_keras(obs, model, m_pre_proc)
                print(out)
                print('Sum of prob:', np.sum(out))
                # unit_test_predict_proba_keras(data, model, m_pre_proc)
                # unit_test_predict_proba_multiple_obs(data, model)
            except Exception as e:
                error_code = 'model_prediction_failed'
                error_log = (str(e))
                print(error_code)
                print(error_log)
        except Exception as e:
            error_code = 'model_loading_failed'
            error_log = (str(e))
            print(error_code)
            print(error_log)
    # TODO: Add logging
    # TODO: utf-8 input
    # TODO: inputs handelling **kwrgs
    return None


if __name__ == "__main__":
    start_time = timeit.default_timer()
    main()
    print('Time taken (sec):', round(timeit.default_timer() - start_time))
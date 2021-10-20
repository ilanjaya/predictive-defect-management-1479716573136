# -*- coding: utf-8 -*-
"""
@author: nilesh

Tasks: Completed --------------------------------------------------------------

Tasks: Pending ----------------------------------------------------------------

Refrences: --------------------------------------------------------------------

Refrences Data: ---------------------------------------------------------------

External Repos: ---------------------------------------------------------------
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

from sklearn.preprocessing import LabelEncoder
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.decomposition import TruncatedSVD
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
from keras.layers import Convolution1D, AveragePooling1D, Conv1D, Input
from keras.layers import GlobalAveragePooling1D
from keras.preprocessing.text import Tokenizer
from keras.optimizers import Nadam

# User Inputs -----------------------------------------------------------------
path_base = os.path.dirname(os.path.dirname(os.path.realpath(__file__))) 

if platform.uname()[1] == 'AIIGULCE001':
    mkl.set_num_threads(40)
elif platform.system() == 'Linux':
    mkl.set_num_threads(8)
else:
    mkl.set_num_threads(4)
    

fl_inp_pp_train = 'aa_TL_DRM_IDA_Traget.csv.gz'
fl_inp_pp_test = 'aa_TL_DRM_IDA_Traget.csv.gz'

fl_sub_input = None
fl_word_vectors_zip = 'ab_glove.6B.300d.zip'
fl_word_vectors = 'ab_glove.6B.300d.txt'

n_output_hidden_units = 10
cv_n_fold = 10
cv_n_fold_dl = 0

# Set Base Path----------------------------------------------------------------
os.chdir(path_base)
sys.path.append(path_base)
from q_scripts.a_class_func_dir import DirStructure, Job, Data, Word2VecTransformer
from q_scripts.a_class_func_dir import TextToNumericSequence, PadNumericSequence
from q_scripts.e_feature_eng import get_tfidf_truncSVD_features, load_tSVD_balanced_data


# Read Config File: -----------------------------------------------------------
fld = DirStructure('config.ini')


def aa_tfidf_MaxEnt(data):
    job = Job('aa_tfidf_MaxEnt', cv = cv_n_fold)
    pipeline = Pipeline(steps=[("tfidf", TfidfVectorizer(stop_words = 'english',
                                                         max_features = 2000,
                                                         min_df = 5)),
                               ('m', LogisticRegression())])
    parameters = dict(tfidf__norm = ['l2'],
                      tfidf__ngram_range = [(1, 2)],
                      m__C = [0.001, 0.01, 0.1, 1, 10])
    job.run(pipeline, parameters, data)
    return None
    
def ab_tfidf_elasticnet(data):
    job = Job('ab_tfidf_elasticnet', cv = cv_n_fold)
    pipeline = Pipeline(steps=[("tfidf", TfidfVectorizer(stop_words = 'english')),
                               ('elnet', SGDClassifier(penalty="elasticnet"))])
    parameters = dict(tfidf__norm = ['l2'],
                      tfidf__ngram_range = [(1, 2), (1, 3)], # ,      # [(1, 3)]
                      elnet__alpha = [1e-5, 1e-4, 1e-3, 1e-2],  # [1e-5, 1e-4, 1e-3, 1e-2, 1e-1]
                      elnet__l1_ratio = [0.1, 0.5, 0.8, 0.9, 0.99]) # [0.1, 0.5, 0.8, 0.9, 0.99]
    job.run(pipeline, parameters, data)
    return None
    
def ac_truncSVD_GBM(data):
    job = Job('ac_truncSVD_GBM', cv = cv_n_fold)
    data_tSVD = copy.deepcopy(data)
    data_tSVD = get_tfidf_truncSVD_features(data_tSVD, fs_text = data.fs_ind, 
                                            ngram_range = (1, 2),
                                n_components = 2000, verbose=1)
    # n_components = 2000 --> variance explained = 
    pipeline = Pipeline(steps=[('gbm', GradientBoostingClassifier())])
    parameters = dict(gbm__n_estimators = [100, 300, 500])
    job.run(pipeline, parameters, data_tSVD)
    return None
    
def ba_GBM(data, name = ''):
    job = Job('ba_GBM_' + name, cv = cv_n_fold)
    pipeline = Pipeline(steps=[('gbm', GradientBoostingClassifier())])
    parameters = dict(gbm__n_estimators = [2])
    job.run(pipeline, parameters, data)
    return None
    
    
def ad_truncSVD_randomForest(data):    
    job = Job('ad_truncSVD_randomForest', cv = cv_n_fold)
    data_tSVD = copy.deepcopy(data)
    data_tSVD = get_tfidf_truncSVD_features(data_tSVD, fs_text = data.fs_ind, 
                                            ngram_range = (1, 2),
                                n_components = 2000, verbose=1)
    # n_components = 2000 --> variance explained = 
    pipeline = Pipeline(steps=[('rf', RandomForestClassifier())])
    parameters = dict(rf__n_estimators = [10, 30, 90, 270, 810],
                      rf__max_features = [60, 80, 'auto', 120, 140],
                      rf__max_depth = [5, 10, 15, None],
                      rf__min_samples_split = [2, 5, 10])
    job.run(pipeline, parameters, data_tSVD)
    return None

def ae_truncSVD_randomForest(data):
    data_tSVD = copy.deepcopy(data)
    data_tSVD = get_tfidf_truncSVD_features(data_tSVD, fs_text = data.fs_ind, 
                                            ngram_range = (1, 2),
                                n_components = 2000, verbose=1)
    # n_components = 2000 --> variance explained = 
    job = Job('ad_truncSVD_randomForest', cv = cv_n_fold)
    pipeline = Pipeline(steps=[('rf', RandomForestClassifier())])
    parameters = dict(rf__n_estimators = [10, 30, 90, 270, 810],
                      rf__max_features = [60, 80, 'auto', 120, 140],
                      rf__max_depth = [5, 10, 15, None],
                      rf__min_samples_split = [2, 5, 10])
    job.run(pipeline, parameters, data_tSVD)
    return None
    
def ae_tfidf_BayesianRidge(data):
    job = Job('ae_tfidf_BayesianRidge')
    pipeline = Pipeline(steps=[("tfidf", TfidfVectorizer(stop_words = 'english')),
                               ('br', BayesianRidge())])
    parameters = dict(tfidf__norm = ['l2'],
                      tfidf__ngram_range = [(1, 2)])
    job.run(pipeline, parameters, data)
    return None    

def af_vecAvg_MaxEnt(data):
    job = Job('af_vecAvg_MaxEnt', cv = cv_n_fold)
    pipeline = Pipeline(steps=[("vecAvg",
                                Word2VecTransformer(fl_word_vectors_zip,
                                                    fl_word_vectors,
                                                    dim = 300,
                                                    all_text_data = 
                                                     list(data.df[data.fs_ind]))),
                               ('m', LogisticRegression())])
    parameters = dict(m__C = [0.001, 0.01, 0.1, 1, 10])
    job.run(pipeline, parameters, data)
    return None
    
def ag_vecAvg_randomForest(data):
    job = Job('ag_vecAvg_randomForest', cv = cv_n_fold)
    pipeline = Pipeline(steps=[("vecAvg",
                                Word2VecTransformer(fl_word_vectors_zip,
                                                    fl_word_vectors,
                                                    dim = 300,
                                                    all_text_data = 
                                                     list(data.df[data.fs_ind]))),
                               ('rf', RandomForestClassifier())])
    parameters = dict(rf__n_estimators = [30, 90, 270])
    job.run(pipeline, parameters, data)
    return None

def ab_tfidf_elasticnet_OutputCode(data):
    job = Job('ab_tfidf_elasticnet_OutputCode', cv = cv_n_fold)
    pipeline = Pipeline(steps=[("tfidf", TfidfVectorizer(stop_words = 'english',
                                                         min_df = 5)),
                               ('elnet', OutputCodeClassifier(
                               SGDClassifier(penalty="elasticnet"),
                               code_size = 100))])
    parameters = dict(tfidf__norm = ['l2'],
                      tfidf__ngram_range = [(1, 2)], # ,      # [(1, 3)]
                      elnet__estimator__alpha = [0.0001],  # [1e-5, 1e-4, 1e-3, 1e-2, 1e-1]
                      elnet__estimator__l1_ratio = [0.1]) # [0.1, 0.5, 0.8, 0.9, 0.99]
    job.run(pipeline, parameters, data)
    return None
    
def aa_tfidf_MaxEnt_OutputCode(data):
    job = Job('aa_tfidf_MaxEnt_OutputCode', cv = cv_n_fold)
    pipeline = Pipeline(steps=[("tfidf", TfidfVectorizer(stop_words = 'english',
                                                         max_features = 2000,
                                                         min_df = 5)),
                               ('m', OutputCodeClassifier(LogisticRegression(),
                                                          code_size = 10))])
    parameters = dict(tfidf__norm = ['l2'],
                      tfidf__ngram_range = [(1, 2)],
                      m__estimator__C = [0.01])
    job.run(pipeline, parameters, data)
    return None

def af_vecAvg_MaxEnt_OutputCode(data):
    job = Job('af_vecAvg_MaxEnt_OutputCode', cv = cv_n_fold)
    pipeline = Pipeline(steps=[("vecAvg", Word2VecTransformer(fld.get_path(fld.model_meta_data, fl_word_vectors), 
                                                              dim = 300,
                                                              all_text_data = list(data.df[data.fs_ind]))),
                               ('m', OutputCodeClassifier(LogisticRegression(),
                                                          code_size = 10))])
    parameters = dict(m__estimator__C = [0.01])
    job.run(pipeline, parameters, data)
    return None

# Deeplearning Pipelines: -----------------------------------------------------
def cnn(max_features = 20000, embedding_dims = 64,
                 max_seq_len = 300, nb_filter = 500, filter_length = 2,
                 hidden_dims1=1000, hidden_dims2=100):
    
    model = Sequential()
    model.add(Embedding(max_features,
                        embedding_dims,
                        input_length=max_seq_len,
                        dropout=0.25))
    model.add(Convolution1D(nb_filter=nb_filter,
                            filter_length=filter_length,
                            border_mode='valid',
                            activation='relu',
                            subsample_length=1))
    model.add(MaxPooling1D(pool_length=model.output_shape[1]))
    model.add(Flatten())
    model.add(Dense(hidden_dims1))
    model.add(Dropout(0.5))
    model.add(Activation('relu'))
    model.add(Dense(hidden_dims2))
    model.add(Dropout(0.5))
    model.add(Activation('relu'))
    model.add(Dense(n_output_hidden_units))
    model.add(Activation('softmax'))
    optm = Nadam(lr=0.00002, schedule_decay=0.00004)
    model.compile(loss='categorical_crossentropy',
                  optimizer=optm, metrics = ['accuracy'])              
    return model
    
    
def ah_embedding_cnn(data):
    job = Job('ah_embedding_cnn', cv = cv_n_fold_dl, n_threads = 1)
    cnn_model = KerasClassifier(build_fn=cnn, batch_size=32, nb_epoch=80,
                               validation_split = 0.1, verbose = 1)
    pipeline = Pipeline(steps=[('txt_to_seq', TextToNumericSequence()),
                               ('padd_seq', PadNumericSequence()),
                               ('cnn', cnn_model)])
    parameters = dict(txt_to_seq__n_max_features = [20000],
                      padd_seq__max_seq_len = [500],
                      cnn__max_seq_len = [500],
                      cnn__hidden_dims1 = [1000])
    job.run(pipeline, parameters, data)
    return None


def lstm(max_features = 20000, embedding_dims = 128):
    model = Sequential()
    model.add(Embedding(max_features, embedding_dims, dropout=0.2))
    model.add(LSTM(128, dropout_W=0.2, dropout_U=0.2))
    model.add(Dense(n_output_hidden_units))
    model.add(Activation('softmax'))
    model.compile(loss='categorical_crossentropy',
                  optimizer='rmsprop', metrics = ['accuracy'])
    return model
    
def ai_embedding_lstm(data):
    job = Job('ai_embedding_lstm', cv = cv_n_fold_dl, n_threads = 1)
    lstm_model = KerasClassifier(build_fn=lstm, batch_size=32, nb_epoch=50,
                               validation_split = 0.1, verbose = 1)
    pipeline = Pipeline(steps=[('txt_to_seq', TextToNumericSequence()),
                               ('padd_seq', PadNumericSequence()),
                               ('lstm', lstm_model)])
    parameters = dict(txt_to_seq__n_max_features = [50000],
                      padd_seq__max_seq_len = [750],
                      lstm__embedding_dims = [128])
    job.run(pipeline, parameters, data)
    return None

def fasttext(max_features = 20000, embedding_dims = 100, max_seq_len = 300):
    model = Sequential()
    model.add(Embedding(max_features,
                        embedding_dims,
                        input_length=max_seq_len))
    model.add(AveragePooling1D(pool_length=model.output_shape[1]))
    model.add(Flatten())
    model.add(Dense(n_output_hidden_units))
    model.add(Activation('softmax'))
    model.compile(loss='categorical_crossentropy',
                  optimizer='adam', metrics = ['accuracy'])      
    return model
    
def aj_embedding_fasttext(data):
    job = Job('aj_embedding_fasttext', cv = cv_n_fold_dl, n_threads = 1)
    ft_model = KerasClassifier(build_fn=fasttext, batch_size=32, nb_epoch=5,
                               validation_split = 0.1, verbose = 1)
    pipeline = Pipeline(steps=[('txt_to_seq', TextToNumericSequence()),
                               ('padd_seq', PadNumericSequence()),
                               ('ft', ft_model)])
    # TODO: add ngram features based on the paper
    parameters = dict(txt_to_seq__n_max_features = [20000],
                      padd_seq__max_seq_len = [300],
                      ft__max_seq_len = [300],
                      ft__embedding_dims = [100])
    job.run(pipeline, parameters, data)
    return None

def cnn_lstm(max_features = 20000, embedding_dims = 50, max_seq_len = 300, 
             nb_filter = 250, filter_length = 2, pool_length = 3, lstm_output_size = 70):                  
    model = Sequential()
    model.add(Embedding(max_features, embedding_dims, input_length=max_seq_len))
    model.add(Dropout(0.25))
    model.add(Convolution1D(nb_filter=nb_filter,
                            filter_length=filter_length,
                            border_mode='valid',
                            activation='relu',
                            subsample_length=1))
    model.add(MaxPooling1D(pool_length=pool_length))
    model.add(LSTM(lstm_output_size))

    model.add(Dense(1, activation='linear'))
    model.compile(loss='mae',
                  optimizer='adam')              
    return model
    
def ak_embedding_cnn_lstm(data):
    job = Job('ak_embedding_cnn_lstm', cv = cv_n_fold_dl, n_threads = 1)
    cnn_lstm_model = KerasClassifier(build_fn=cnn_lstm, batch_size=32, nb_epoch=10,
                               validation_split = 0.1, verbose = 1)
    pipeline = Pipeline(steps=[('txt_to_seq', TextToNumericSequence()),
                               ('padd_seq', PadNumericSequence()),
                               ('cnn_lstm', cnn_lstm_model)])
    parameters = dict(txt_to_seq__n_max_features = [20000],
                      padd_seq__max_seq_len = [300],
                      cnn_lstm__embedding_dims = [50])
    job.run(pipeline, parameters, data)
    return None

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
    
def glove_cnn_lstm(max_features = 20000, embedding_dims = 300, max_seq_len = 300,
              nb_filter = 128, filter_length = 2, pool_length = 3,
              lstm_output_size = 100):
    embedding_layer = Embedding(max_features + 1,
                                embedding_dims,
                                weights=[embedding_matrix],
                                input_length=max_seq_len,
                                trainable=True)
    model = Sequential()
    model.add(embedding_layer)
    model.add(Dropout(0.25))
    model.add(Convolution1D(nb_filter=nb_filter,
                            filter_length=filter_length,
                            border_mode='valid',
                            activation='relu',
                            subsample_length=1))
    model.add(MaxPooling1D(pool_length=pool_length))
    model.add(LSTM(lstm_output_size))
    model.add(Dense(n_output_hidden_units))
    model.add(Activation('softmax'))
    optm = Nadam(lr=0.002, schedule_decay=0.004)
    model.compile(loss='categorical_crossentropy',
                  optimizer=optm, metrics = ['accuracy'])      
    return model
    
def al_glove_cnn_lstm(data):
    job = Job('al_glove_cnn_lstm', cv = cv_n_fold_dl, n_threads = 1)
    global embedding_matrix
    embedding_matrix = create_embedding_matrix(data.df[data.fs_ind], max_features=20000,
                                               embedding_dims = 300)
    glove_cnn_lstm_m = KerasClassifier(build_fn=glove_cnn_lstm, batch_size=64, 
                                       nb_epoch=10,
                               validation_split = 0.1, verbose = 1)
    pipeline = Pipeline(steps=[('txt_to_seq', TextToNumericSequence()),
                               ('padd_seq', PadNumericSequence()),
                               ('g_c_l', glove_cnn_lstm_m)])
    parameters = dict(txt_to_seq__n_max_features = [20000],
                      padd_seq__max_seq_len = [300],
                      g_c_l__max_seq_len = [300],
                      g_c_l__embedding_dims = [300])
    job.run(pipeline, parameters, data)
    return None


def glove_fasttext(max_features = 20000, embedding_dims = 300, 
                   max_seq_len = 300):
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
    
def am_glove_fasttext(data):
    job = Job('am_glove_fasttext', cv = cv_n_fold_dl, n_threads = 1, 
              model_package = 'keras')
    max_features = 40000
    max_seq_len = 700
    embedding_dims = 300
    batch_size = 256
    nb_epoch = 200
    
    global embedding_matrix
    embedding_matrix = create_embedding_matrix(data.df[data.fs_ind], 
                                               max_features=max_features,
                                               embedding_dims = embedding_dims)
    m = KerasClassifier(build_fn=glove_fasttext, batch_size=batch_size,
                         validation_split = 0.1, nb_epoch=nb_epoch, verbose = 1)
    pipeline = Pipeline(steps=[('txt_to_seq', TextToNumericSequence()),
                               ('padd_seq', PadNumericSequence()),
                               ('m', m)])
    parameters = dict(txt_to_seq__n_max_features = [max_features],
                      padd_seq__max_seq_len = [max_seq_len],
                      m__max_features = [max_features],
                      m__max_seq_len = [max_seq_len],
                      m__embedding_dims = [embedding_dims])
    job.run(pipeline, parameters, data)
    return None


def pre_process_data_for_deep_learning(data, fs_text = 'text', verbose = 0):
    pipeline = Pipeline(steps=[('txt_to_seq', TextToNumericSequence()),
                               ('padd_seq', PadNumericSequence())])
    p = pipeline.fit(data.df[fs_text].values)
    text_num = p.transform(data.df[fs_text].values)
    print(text_num)
    text_num = pd.DataFrame(text_num)
    fe_columns = ['text_numeric']
    text_num.columns = fe_columns

    data.df = data.df.join(text_num)
    data.fs_ind = fe_columns
    return data
    
    
def glove_cnn(max_features = 20000, embedding_dims = 100, max_seq_len = 300,
              nb_filter = 128, filter_length = 2, pool_length = 3):
    embedding_layer = Embedding(max_features + 1,
                                embedding_dims,
                                weights=[embedding_matrix],
                                input_length=max_seq_len,
                                trainable=True)
    sequence_input = Input(shape=(max_seq_len,), dtype='int32')
    embedded_sequences = embedding_layer(sequence_input)
    x = Conv1D(128, filter_length, activation='relu')(embedded_sequences)
    x = MaxPooling1D(pool_length)(x)
    x = Conv1D(nb_filter, filter_length, activation='relu')(x)
    x = MaxPooling1D(pool_length)(x)
    x = Conv1D(128, filter_length, activation='relu')(x)
    x = MaxPooling1D(pool_length)(x)
    x = Flatten()(x)
    x = Dense(128, activation='relu')(x)
    preds = Dense(10, activation='softmax')(x)
    model = Model(sequence_input, preds)
    model.compile(loss='categorical_crossentropy',
                  optimizer='adam', metrics = ['accuracy'])  
    return model
    
def an_glove_cnn(data):
    job = Job('an_glove_cnn', cv = cv_n_fold_dl, n_threads = 1)
    max_features = 20000
    max_seq_len = 300
    embedding_dims = 300
    batch_size = 64
    nb_epoch = 10
    global embedding_matrix
    embedding_matrix = create_embedding_matrix(data.df[data.fs_ind], max_features=max_features,
                                               embedding_dims = embedding_dims)
    m = KerasClassifier(build_fn=glove_cnn, batch_size=batch_size, nb_epoch=nb_epoch,
                               validation_split = 0.1, verbose = 1)
    pipeline = Pipeline(steps=[('txt_to_seq', TextToNumericSequence()),
                               ('padd_seq', PadNumericSequence()),
                               ('m', m)])
    parameters = dict(txt_to_seq__n_max_features = [max_features],
                      padd_seq__max_seq_len = [max_seq_len],
                      m__max_features = [max_features],
                      m__max_seq_len = [max_seq_len],
                      m__embedding_dims = [embedding_dims])
    job.run(pipeline, parameters, data)
    return None
    
def multi_fltr_glove_cnn(max_features = 20000, embedding_dims = 300, 
                         max_seq_len = 300, nb_filter = 128, filter_length = 2, 
                         pool_length = 3):
    embedding_layer = Embedding(max_features + 1,
                                embedding_dims,
                                weights=[embedding_matrix],
                                input_length=max_seq_len,
                                trainable=True)
    filter_sizes = (2, 3, 4)          
    graph_in = Input(shape=(max_seq_len, embedding_dims))
    convs = []
    for fsz in filter_sizes:
        conv = Convolution1D(nb_filter=nb_filter,
                             filter_length=fsz,
                             border_mode='valid',
                             activation='relu',
                             subsample_length=1)(graph_in)
        pool = MaxPooling1D(pool_length=2)(conv)
        flatten = Flatten()(pool)
        convs.append(flatten)
    if len(filter_sizes)>1:
        out = Merge(mode='concat')(convs)
    else:
        out = convs[0]
    graph = Model(input=graph_in, output=out)

    model = Sequential()
    model.add(embedding_layer)
    model.add(Dropout(0.25, input_shape=(max_seq_len, embedding_dims)))
    model.add(graph)
    model.add(Dense(150))
    model.add(Dropout(0.5))
    model.add(Activation('relu'))
    model.add(Dense(n_output_hidden_units))
    model.add(Activation('softmax'))
    model.compile(loss='categorical_crossentropy',
                  optimizer='rmsprop', metrics = ['accuracy'])  
    return model
    
def ao_multi_fltr_glove_cnn(data):
    job = Job('ao_multi_fltr_glove_cnn', cv = cv_n_fold_dl, n_threads = 1)
    max_features = 20000
    max_seq_len = 300
    embedding_dims = 300
    batch_size = 64
    nb_epoch = 10
    global embedding_matrix
    embedding_matrix = create_embedding_matrix(data.df[data.fs_ind], max_features=max_features,
                                               embedding_dims = embedding_dims)
    m = KerasClassifier(build_fn=multi_fltr_glove_cnn, batch_size=batch_size, nb_epoch=nb_epoch,
                               validation_split = 0.1, verbose = 1)
    pipeline = Pipeline(steps=[('txt_to_seq', TextToNumericSequence()),
                               ('padd_seq', PadNumericSequence()),
                               ('m', m)])
    parameters = dict(txt_to_seq__n_max_features = [max_features],
                      padd_seq__max_seq_len = [max_seq_len],
                      m__max_features = [max_features],
                      m__max_seq_len = [max_seq_len],
                      m__embedding_dims = [embedding_dims])
    job.run(pipeline, parameters, data)
    return None
    
# Ensamble Pipelines: ---------------------------------------------------------
def ea_regression(data):
    job = Job('ea_regression', cv = cv_n_fold, ensamble_model_job = True)
    pipeline = Pipeline(steps=[('reg', SGDClassifier())])
    parameters = dict(reg__alpha = [0.0001])
    job.run(pipeline, parameters, data)
    return None

def eb_randomForest(data):
    job = Job('eb_randomForest', cv = cv_n_fold, ensamble_model_job = True)
    pipeline = Pipeline(steps=[('rf', RandomForestClassifier())])
    parameters = dict(rf__n_estimators = [3, 9, 27])
    job.run(pipeline, parameters, data)
    return None
    
def ec_AdaBoost_Extratree(data):
    job = Job('ec_AdaBoost_Extratree', cv = cv_n_fold, ensamble_model_job = True)
    pipeline = Pipeline(steps=[('ada_extraTree', 
                                AdaBoostRegressor(ExtraTreesRegressor()))])
    parameters = dict(ada_extraTree__base_estimator__n_estimators = [10])
    job.run(pipeline, parameters, data)
    return None
    
def ed_GBM(data):
    job = Job('ed_GBM', cv = cv_n_fold, ensamble_model_job = True)
    pipeline = Pipeline(steps=[('gbm', GradientBoostingClassifier())])
    parameters = dict(gbm__n_estimators = [30, 100, 300])
    job.run(pipeline, parameters, data)
    return None
    
def ef_XGBoost(data):
    '''
    job = Job('ef_XGBoost', cv = cv_n_fold, ensamble_model_job = True)
    pipeline = Pipeline(steps=[('xgb', XGBRegressor(silent = False))])
    parameters = dict(xgb__n_estimators = [100],
                      xgb__objective = ['reg:linear'], 
                      xgb__learning_rate = [0.1],
                      xgb__max_depth = [3],
                      xgb__subsample = [1],
                      xgb__colsample_bytree = [1])
    job.run(pipeline, parameters, data)
    '''
    return None

    
    
# Run Code: -------------------------------------------------------------------
def main():
    data = Data(pd_or_np = 'pd', fl_submission_input = fl_sub_input)
    
    if False:
        #### inbalanced (original data)
        data.load_pre_processed(fl_inp_pp_train, fl_inp_pp_test,
                                nrows_train = None, nrows_test = 100)
        data.set_idx_train_valid_test_custom(n_train = 13871, n_valid = 12305)
        data.set_fs(fs_target = ['label'], fs_id = ['id'],
                    fs_ind = 'text')
        
        x = data.df[data.fs_target].values.ravel()
        print('Target distribution top 5:\n', data.df.label.value_counts()[0:5])
        le = LabelEncoder().fit(x)
        data.df[data.fs_target] = le.transform(x)
        data.le =  le
        data.describe()
        print('Building Model')
        
        
        # aa_tfidf_MaxEnt(data)
        # score_LB: None  , score_train: 0.6484, score_cv: 0.3783, score_valid: 0.2398, score2_train: 0.7857, score2_cv: None  , score2_valid: 0.503 , param_best: {'tfidf__ngram_range': (1, 2), 'm__C': 10, 'tfidf__norm': 'l2'}, time_model_training_min: 1.783
        # aa_tfidf_MaxEnt_OutputCode(data)
        # score_LB: None  , score_train: 0.1843, score_cv: 0.1069, score_valid: 0.0   , score2_train: 0.577 , score2_cv: None  , score2_valid: 0.356 , param_best: {'tfidf__ngram_range': (1, 2), 'm__estimator__C': 0.01, 'tfidf__norm': 'l2'}, time_model_training_min: 1.583
        ## ac_truncSVD_GBM(data)
        ad_truncSVD_randomForest(data)
        # score_LB: None  , score_train: 0.4564, score_cv: 0.2244, score_valid: 0.0004, score2_train: 0.7033, score2_cv: None  , score2_valid: 0.3563, param_best: {'rf__min_samples_split': 2, 'rf__max_features': 140, 'rf__max_depth': 10, 'rf__n_estimators': 270}, time_model_training_min: 585.833
        # score_LB: None  , score_train: 0.4362, score_cv: 0.224 , score_valid: 0.0017, score2_train: 0.6951, score2_cv: None  , score2_valid: 0.3572, param_best: {'rf__max_depth': 10, 'rf__n_estimators': 810, 'rf__max_features': 140, 'rf__min_samples_split': 2}, time_model_training_min: 642.8
        # af_vecAvg_MaxEnt(data)
        # score_LB: None  , score_train: 0.0   , score_cv: 0.0   , score_valid: 0.0   , score2_train: 0.5116, score2_cv: None  , score2_valid: 0.356 , param_best: {'m__C': 0.001}, time_model_training_min: 9.317

        # ag_vecAvg_randomForest(data)
        ## af_vecAvg_MaxEnt_OutputCode(data) 
        
        am_glove_fasttext(data)
        # score_LB: None  , score_train: 0.5408, score_cv: 0.384 , score_valid: 0.1087, score2_train: 0.7332, score2_cv: None  , score2_valid: 0.4333, param_best: {'m__max_seq_len': 700, 'm__embedding_dims': 300, 'txt_to_seq__n_max_features': 40000, 'm__max_features': 40000, 'padd_seq__max_seq_len': 700}, time_model_training_min: 255.333

        # aj_embedding_fasttext(data)
        # ao_multi_fltr_glove_cnn(data)
        # ah_embedding_cnn(data)
        # ai_embedding_lstm(data)
        ## an_glove_cnn(data)
        # al_glove_cnn_lstm(data)
        # ai_embedding_lstm(data)
        ac_truncSVD_GBM(data)
        # score_LB: None  , score_train: 0.9723, score_cv: 0.2603, score_valid: 0.1004, score2_train: 0.9822, score2_cv: None  , score2_valid: 0.4304, param_best: {'gbm__n_estimators': 500}, time_model_training_min: 803.15

        ### Ensamble
        data.load_pre_processed(fl_inp_pp_train, fl_inp_pp_test,
                                nrows_train = None, nrows_test = 100)
        data.set_idx_train_valid_test_custom(n_train = 13871, n_valid = 12305)
        data.create_ensamble_data()
        data.load_ensamble_data()
        data.set_idx_train_valid_test_custom(n_train = 13871, n_valid = 12305)
        data.set_fs(fs_target = ['label'], fs_id = ['id'],
                    fs_ind = 'text')
        ef_XGBoost(data)
    else:
        data_base = Data(pd_or_np = 'pd', fl_submission_input = fl_sub_input)
        data_base.load_pre_processed(fl_inp_pp_train, fl_inp_pp_test,
                                nrows_train = None, nrows_test = 100)
        data_base.set_idx_train_valid_test_custom(n_train = 13871, n_valid = 12305, 
                                                  n_test = 100)
        data_base.set_fs(fs_target = ['label'], fs_id = ['id'],
                    fs_ind = 'text')
        import pickle
        with open(fld.get_path(fld.data_featur_eng, 'tfidf_model.pkl'), 'rb') as fl:
            tfidf_model = pickle.load(fl)    
        with open(fld.get_path(fld.data_featur_eng, 'tsvd_model.pkl'), 'rb') as fl:
            tsvd_model = pickle.load(fl)
        X_valid_tsvd = tsvd_model.transform(tfidf_model.transform(data_base.X_valid()))
        y_valid_tsvd = data_base.y_valid()
        balanced_data = load_tSVD_balanced_data()
        
        X_train_balanced = []
        y_train_balanced = []
        for var in balanced_data.keys():
            print('Building Model for var:', var)
            subset = balanced_data[var]
            X_train_balanced.append(subset['X_train_balanced'])
            y_train_balanced.append(subset['y_train_balanced'])
            X_test_balanced = subset['X_test_balanced']
            y_test_balanced = subset['y_test_balanced']
            print(X_test_balanced.shape, y_test_balanced.shape)
        X_train_balanced = np.concatenate(X_train_balanced)
        y_train_balanced = np.concatenate(y_train_balanced)
        print(X_train_balanced.shape, y_train_balanced.shape,
              X_test_balanced.shape, y_test_balanced.shape,
              X_valid_tsvd.shape, y_valid_tsvd.shape)
        print(np.bincount(y_train_balanced))
        '''
        data = Data(pd_or_np = 'np', fl_submission_input = fl_sub_input)
        data.create_from_np_objects(X_valid_tsvd ,
                                    y_valid_tsvd)
        data.set_idx_train_valid_test_custom(n_train = subset['y_train_balanced'].shape[0],
                                             n_valid = subset['y_test_balanced'].shape[0],
                                             n_test = data_base.X_valid().shape[0])
        print(data.describe())
        ba_GBM(data, name = str(var))
        '''
        
        '''
        data_base = Data(pd_or_np = 'pd', fl_submission_input = fl_sub_input)
        data_base.load_pre_processed(fl_inp_pp_train, fl_inp_pp_test,
                                nrows_train = None, nrows_test = 100)
        data_base.set_idx_train_valid_test_custom(n_train = 13871, n_valid = 12305, 
                                                  n_test = 100)
        data_base.set_fs(fs_target = ['label'], fs_id = ['id'],
                    fs_ind = 'text')
        import pickle
        with open(fld.get_path(fld.data_featur_eng, 'tfidf_model.pkl'), 'rb') as fl:
            tfidf_model = pickle.load(fl)    
        with open(fld.get_path(fld.data_featur_eng, 'tsvd_model.pkl'), 'rb') as fl:
            tsvd_model = pickle.load(fl)
            
        X_valid_tsvd = tsvd_model.transform(tfidf_model.transform(data_base.X_valid()))
        y_valid_tsvd = data_base.y_valid()
        balanced_data = load_tSVD_balanced_data()
        for var in balanced_data.keys():
            print('Building Model for var:', var)
            subset = balanced_data[var]
            data = Data(pd_or_np = 'np', fl_submission_input = fl_sub_input)
            data.create_from_np_objects(subset['X_train_balanced'],
                                        subset['y_train_balanced'],
                                        subset['X_test_balanced'],
                                        subset['y_test_balanced'],
                                        X_valid_tsvd ,
                                        y_valid_tsvd)
            data.set_idx_train_valid_test_custom(n_train = subset['y_train_balanced'].shape[0],
                                                 n_valid = subset['y_test_balanced'].shape[0],
                                                 n_test = data_base.X_valid().shape[0])
            print(data.describe())
            ba_GBM(data, name = str(var))
        '''
            
        #  tensorboard --logdir=/ext/ce/Nilesh/_CognitiveTesting/CognitiveTesting/IDA_DRM_Trigger/i_model_scoring/am_glove_fasttext/TensorBoard --port=9000
    return None


if __name__ == "__main__":
    start_time = timeit.default_timer()
    main()
    print('Time taken (sec):', round(timeit.default_timer() - start_time))

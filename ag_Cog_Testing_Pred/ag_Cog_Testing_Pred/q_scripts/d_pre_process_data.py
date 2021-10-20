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
import csv
import numpy as np
import glob
from nltk.corpus import stopwords 
import re


# User Inputs: ----------------------------------------------------------------
path_base = os.path.dirname(os.path.dirname(os.path.realpath(__file__))) 
fl_train = ''
fl_train_sheet_name = ''
fl_pre_proc = ''

fld_data = 'TELCO_DATA'
fld_data_validation = 'Batch 1-2-3'
fl_pre_proc_data = 'aa_TL_DRM_IDA_Traget.csv.gz'

# Set Base Path: --------------------------------------------------------------
path_base = os.path.dirname(os.path.dirname(os.path.realpath(__file__))) 
os.chdir(path_base)
sys.path.append(path_base)
from q_scripts.a_class_func_dir import DirStructure


# Read Config File: -----------------------------------------------------------
fld = DirStructure('config.ini')

# Preprocess Functions: -------------------------------------------------------
def words_data(col):
    col = col.encode('utf-8').strip()

    str_lines = col.splitlines()
    
    #regexp = re.compile('\S+ <\S+>, [0-9]+/[0-9]+/[0-9]+:(.*)$')
    regexp = re.compile('.*<\S+>, [0-9]+/[0-9]+/[0-9]+:(.*)$')
    outputList = list()
    
    for line in str_lines:
        line = line.decode().strip()
        if len(line) > 0:
            if regexp.search(line):
                outputList.append(regexp.search(line).group(1).strip())
            else:
                outputList.append(line)
    
    clean_text = " ".join(map(str, outputList))
    clean_text = re.sub("[^a-zA-Z0-9]", " ", clean_text)
    clean_text = re.sub('[0-9]+', " ", clean_text)
    clean_text = re.sub(r'\s+', ' ', clean_text)
    words = clean_text.lower().split()
    
    #remove tester details 
    try:
        if(words[0] == 'tester'):
            i=0
            for word in words:
                i=i+1
                if(word == 'description'):
                    words = words[i:len(words)-1]
                    break
    except:
        print('------------------------------------------------------')
        print(words)
        print(col)
        print('------------------------------------------------------')
        words = col
    clean_text = " ".join(map(str, words))
    clean_text = clean_text.split()
    stops = set(stopwords.words("english"))                  

    meaningful_words = [w for w in clean_text if not w in stops]  
    clean_text = " ".join( meaningful_words )
    
    return(clean_text)

def read_files_into_df(fld_data):
    path_files = os.path.join(fld.get_path(fld.inp_data), fld_data)
    print(path_files)
    allFiles = glob.glob(path_files + "/*.xls") + \
                glob.glob(path_files + "/*.xlsx") + glob.glob(path_files + "/*.csv")
    load_data = pd.DataFrame() 
    list_ = []
    
    for file_ in allFiles:
        print(os.path.join(path_files, file_))
        print(os.path.splitext(file_)[1])
        if os.path.splitext(file_)[1] == '.csv':
            df = pd.read_csv(os.path.join(path_files, file_), 
                         index_col=None, header=0, encoding = 'ISO-8859-1')
        else:
            df = pd.read_excel(os.path.join(path_files, file_), 
                             index_col=None, header=0, encoding = 'ISO-8859-1')       
        list_.append(df)

    load_data = pd.concat(list_)
    print('Number of observations in original data:', len(load_data))
    load_data = load_data[['Problem Details', 'Title', 'Resolution Details', 'DRM Target']]
    # drop test_process category from drm_target not required for IDA usecase
    # load_data = load_data.loc[load_data['DRM Target'] != 'Test Process']
    load_data = load_data.dropna(subset = ['Title', 'Resolution Details', 'DRM Target'])
    print('Number of observations mandatory fields not empty:', len(load_data))
    return load_data
    
def pre_process_data(fld_data, fld_data_validation, fl_pre_proc_data):
    load_data_train = read_files_into_df(fld_data)
    load_data_valid = read_files_into_df(fld_data_validation)
    
    load_data = pd.concat([load_data_train, load_data_valid], ignore_index = True)
    print('Number of observations in selected data:', len(load_data_train))
    print('Number of observations in selected data:', len(load_data_valid))
    print('Number of observations in selected data:', len(load_data))
    
    load_data_input = load_data['Title'].astype(str) + " " +  \
                        load_data['Problem Details'].astype(str) +  \
                        load_data['Resolution Details'].astype(str)
    
    # load_data_input = load_data_input.values.tolist()
    load_data_target =  load_data['DRM Target'].values.tolist()
    
    clean_data_input = [words_data(doc) for doc in load_data_input]
    clean_data_target = [words_data(doc) for doc in load_data_target]
    
    # convert list to numpy array
    df_input = (np.array(clean_data_input)).reshape(len(clean_data_input), 1)
    df_target = (np.array(clean_data_target)).reshape(len(clean_data_target), 1)
    
    # convert numpy array to dataframe
    clean_data_array = np.concatenate((df_input, df_target), axis=1)
    clean_data_df = pd.DataFrame({'Input':clean_data_array[:,0],
                                  'Target':clean_data_array[:,1]})
    
    dat = clean_data_df
    dat.columns = ['text', 'label']
    col_features = ['label', 'text']
    dat = dat[col_features]
    # dat = dat.query('label != "test process"')
    print('Number of observations:', len(dat))
    print(dat.head())
    dat['id'] = range(dat.shape[0])
    path_pre_proc = fld.get_path(fld.inp_data_pre_proc, 
                                 fl_pre_proc_data)
    dat.to_csv(path_pre_proc, index = False, compression='gzip', 
               encoding='utf-8', quoting=csv.QUOTE_NONNUMERIC)
    return None
    
def explore_data(fl):
    path_pre_proc = fld.get_path(fld.inp_data_pre_proc, fl)
    dat = pd.read_csv(path_pre_proc)
    print('Number of classes:', len(set(dat.label)))
    print('Top 10 classes:\n', dat.label.value_counts()[0:10])
    text = list(dat.text)
    len(text)
    words = [ x.split(' ') for x in text]
    word_len = [len(l) for l in words]
    print('max words in document:', max(word_len))
    print('Avg words in document:', np.mean(word_len))
    for max_word_limit in [200, 250, 300, 400, 500, 700, 900, 1000]:
        print('% document less that ', max_word_limit, 'words:',
              sum([1 if n <= max_word_limit else 0 for n in word_len]) / len(word_len))
    words_all = []
    for l in words:
        words_all.extend(l)
    words_all = [x.lower() for x in words_all]
    print('Tokens in corpus:', len(words_all)) 
    # 3 535 339
    print('Vocab size:', len(set(words_all)))
    # 54 903
    print(type(dat.label.value_counts()))
    dat.label.value_counts().to_csv('value_counts.csv')
    print(dat.head(1))
    '''
    Number of observations in selected data: 13871
    Number of observations in selected data: 12305
    Number of observations in selected data: 26176
    '''
    '''
    Number of observations in original data: 17393
    Number of observations mandatory fields not empty: 13871
    Number of observations: 13871
              label                                               text
    0          data  brs itr om order getting kicked error code cus...
    1  test process  iptv shows product fibe tv exist tester name s...
    2          code  brs sft wireless bundle omxw message displayin...
    3          data  brs internet tier redesign om good fttn two ba...
    4          data  drop list npa quebe bccs workstation missing n...
    C:/Users/IBM_ADMIN/Desktop/Box Sync/_Nilesh_Files/_Projects/af_Cog_Testing/_CognitiveTesting/CognitiveTesting/TL_DRM_IDA_Traget/q_scripts/d_pre_process_data.py:135: SettingWithCopyWarning: 
    A value is trying to be set on a copy of a slice from a DataFrame.
    Try using .loc[row_indexer,col_indexer] = value instead
    
    See the caveats in the documentation: http://pandas.pydata.org/pandas-docs/stable/indexing.html#indexing-view-versus-copy
      dat.to_csv(path_pre_proc, index = False, compression='gzip',
    Number of classes: 10
    Top 10 classes:
     test process                 7097
    code                         3879
    environment                  1030
    data                         1029
    requirements                  354
    design                        176
    build package                 128
    user documentation            110
    database                       37
    national language support      31
    Name: label, dtype: int64
    max words in document: 3021
    Avg words in document: 221.623242737
    % document less that  200 words: 0.5899358373585177
    % document less that  250 words: 0.7088890490952346
    % document less that  300 words: 0.7878307259750559
    % document less that  400 words: 0.8813351596856751
    % document less that  500 words: 0.9327373657270565
    % document less that  700 words: 0.9765698219306467
    % document less that  900 words: 0.993079085862591
    % document less that  1000 words: 0.9950976858193353
    Tokens in corpus: 3074136
    Vocab size: 47971
    <class 'pandas.core.series.Series'>
      label                                               text  id
    0  data  brs itr om order getting kicked error code cus...   0
    Time taken: 63.20718077371612
    '''
    
    ''' all data : new data for validation
        Number of observations: 26176
              label                                               text
    0          data  brs itr om order getting kicked error code cus...
    1  test process  iptv shows product fibe tv exist tester name s...
    2          code  brs sft wireless bundle omxw message displayin...
    3          data  brs internet tier redesign om good fttn two ba...
    4          data  drop list npa quebe bccs workstation missing n...
    C:/Users/IBM_ADMIN/Desktop/Box Sync/_Nilesh_Files/_Projects/af_Cog_Testing/_CognitiveTesting/CognitiveTesting/TL_DRM_IDA_Traget/q_scripts/d_pre_process_data.py:152: SettingWithCopyWarning: 
    A value is trying to be set on a copy of a slice from a DataFrame.
    Try using .loc[row_indexer,col_indexer] = value instead
    
    See the caveats in the documentation: http://pandas.pydata.org/pandas-docs/stable/indexing.html#indexing-view-versus-copy
      path_pre_proc = fld.get_path(fld.inp_data_pre_proc,
    Number of classes: 10
    Top 10 classes:
     test process                 11477
    code                          9348
    data                          2153
    environment                   1636
    requirements                   483
    design                         401
    build package                  245
    user documentation             212
    database                       144
    national language support       77
    Name: label, dtype: int64
    max words in document: 3021
    Avg words in document: 185.768681235
    % document less that  200 words: 0.6820751833740831
    % document less that  250 words: 0.7793016503667481
    % document less that  300 words: 0.8444758557457213
    % document less that  400 words: 0.9180929095354523
    % document less that  500 words: 0.956028422982885
    % document less that  700 words: 0.9862087408312958
    % document less that  900 words: 0.9961033007334963
    % document less that  1000 words: 0.9973639975550123
    Tokens in corpus: 4862681
    Vocab size: 86884
    <class 'pandas.core.series.Series'>
      label                                               text  id
    0  data  brs itr om order getting kicked error code cus...   0
    Time taken: 119.99339259542444
    '''
    return None
    
# Run Code: -------------------------------------------------------------------
def main():
    # pre_process_data(fl_train, fl_pre_proc)
    # 74 sec
    # pre_process_telco_data(fl_train_telco, fl_pre_proc_telco)
    # 1.4 sec
    # pre_process_telco_data_noTP(fl_train_telco, fl_pre_proc_telco_no_tp)
    pre_process_data(fld_data, fld_data_validation, fl_pre_proc_data)
    explore_data(fl_pre_proc_data)


if __name__ == "__main__":
    print('Starting preprocessing')
    start_time = timeit.default_timer()
    main()
    print('Time taken:', timeit.default_timer() - start_time)

# -*- coding: utf-8 -*-
"""
@authors: Cognitive Development Team (Nilesh, Mandar, Abhishek, Gomathy, Rahul, Anil)
"""
from __future__ import print_function
import sys
import os
import numpy as np
import mkl
import platform
from sklearn.externals import joblib
from collections import OrderedDict
from nltk.corpus import stopwords 
import re
import json
from flask import Flask, request
from flask_cors import CORS
app = Flask(__name__)
CORS(app)
print('Addressed the CORS issue')

from keras.models import load_model

''' 
[Nilesh] Change Log: 
1. Updated port number to 5004
2. Changed function name and app.route to IDA_DRM_Trigger_Classifier 
3. Change job name to IDA_DRM_Trigger
4. Changed n_output_hidden_units to 38
5. Changed classes variable to names of 38 classes
6. Updated words_data function with new code provided by Abhishek
7. Changed words_data function and removed str_lines = col.splitlines() 
'''


# User Inputs -----------------------------------------------------------------
path_base = os.path.dirname(os.path.dirname(os.path.realpath(__file__))) 
if platform.system() == 'Linux':
    mkl.set_num_threads(8)
else:
    mkl.set_num_threads(4)

n_output_hidden_units = 38
cv_n_fold = 10
cv_n_fold_dl = 0

# Set Base Path----------------------------------------------------------------
os.chdir(path_base)
sys.path.append(path_base)
from q_scripts.a_class_func_dir import DirStructure, Job, Data

# Read Config File: -----------------------------------------------------------
fld = DirStructure('config.ini')
data = Data(pd_or_np = 'pd', fl_submission_input = None)
job = Job('CI_DRM_IDC_Target_noTP', cv = cv_n_fold_dl, n_threads = 1, 
          save_model_tf = True, model_package = None)
path = fld.get_path(fld.model_scoring, job.job_name, 'model_pre_proc.pkl')
m_pre_proc = joblib.load(path)
path = fld.get_path(fld.model_scoring, job.job_name, 'model.h5')
model = load_model(path)

def checkData(obs):
        # Checks the data after preprocessing and returns the status. 
        obs = "".join(obs)
        if obs.strip()=="":
            status = status = {'Error':[{'value':'After preprocessing, the collective inputs are empty. Cannot proceed with classification'}]}
        else:
            status = {'Success':[1]}
        return status

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
    
    #print("\n words ", words)
    #print("\n words[0] ", words[0])

    #remove tester details 
    if(words):
        if(words[0] == 'tester'):
            i=0
            for word in words:
                i=i+1
                if(word == 'description'):
                    words = words[i:len(words)-1]
                    break
    
    clean_text = " ".join(map(str, words))
    clean_text = clean_text.split()
    stops = set(stopwords.words("english"))                  

    meaningful_words = [w for w in clean_text if not w in stops]  
    clean_text = " ".join( meaningful_words )
    
    return(clean_text)
    
def pre_process_observation(load_data_input):
    load_data_input = load_data_input.split()
    clean_data_input = [words_data(doc) for doc in load_data_input]
    return [" ".join(clean_data_input)]

def post_process_observation(title, problem_description, resolution_description, out, status):
    # columns = ['title', 'problem_description', 'resolution_description']
    # values = [title, problem_description, resolution_description]
    classes = ['build package', 'code', 'data', 'database', 'design', 'environment', 'national language support', 'requirements', 'user documentation']

    #if (out!=''):
    out1 = np.ndarray.transpose(out)
    out2 = np.ndarray.tolist(out1) 

    advisoryDetailsList = []

    for i in range(len(classes)):
        info = {
        "defectClass":   classes[i],
        "accuracy": out2[i],
        }
        advisoryDetailsList.append(info)

    advisoryDetailsList = []

    for i in range(len(classes)):
        info = OrderedDict()
        info["defectClass"] = classes[i]
        info["accuracy"] = out2[i][0]
        
        advisoryDetailsList.append(info)

    advisoryDetailsList = sorted(advisoryDetailsList, key=lambda k: k['accuracy'], reverse=True) 

    advisoryDict = dict()
    advisoryDict['advisoryDetailsList'] = advisoryDetailsList[0:4]

    output = json.dumps(advisoryDict)
    return (output)  
      
def predict_class(obs, model):
    #obs = pre_process_observation(obs)
    out = model.predict(obs)
    #out = post_process_observation(out)
    return out

def predict_proba(obs, model):
    #obs = pre_process_observation(obs)
    out = model.predict_proba(obs)
    #out = post_process_observation(out)
    return out   

def predict_proba_keras(obs, model, m_pre_proc):
    #obs = pre_process_observation(obs)
    obs = m_pre_proc.transform(obs)
    out = model.predict_proba(obs)
    #out = post_process_observation(out)
    return out   

@app.route('/CI_DRM_IDC_Target_noTP', methods=['POST'])    
# Run Code: -------------------------------------------------------------------
def CI_DRM_IDC_Target_noTP():
    # Checks for the Title and Problem description fields and throws out an error if it is missing
    #start_time0 = timeit.default_timer()
    ret = {}
    try:
        title = request.form['title']
    except:
        title = ''
    try:
         problem_description = request.form['problem_description']
    except:
        problem_description = ''
    try:
        resolution_description = request.form['resolution_description']
    except:
        resolution_description = ''

    print("\n Title ",title)
    print("\n")
    print("\n problem_description: ",problem_description)
    print("\n")
    print("\n resolution_description : ",resolution_description)
    print("\n")

    if ((not title) and (not problem_description) and (not resolution_description)):
        ret['status'] = 'Missing parameters: title, problem description and resolution description'
        return json.dumps(ret)
    elif not title:
        ret['status'] = 'Missing parameter: title'
        return json.dumps(ret)
    elif not problem_description:
        ret['status'] = 'Missing parameter: problem_description'
        return json.dumps(ret)    
   
    obs = title + " " + problem_description + " " + resolution_description
    print("\nInput Text: ",obs)
    print("\n")
    obs = pre_process_observation(obs)
    print("\nPreprocessed Text : ",obs)
    print("\n")
    status = checkData(obs)
    print("\nStatus: ", status.keys())
    print("\n")
    if (status.keys()=={'Error'}):
        ret['status'] = status
        return json.dumps(ret)
    
    try:
        out = predict_proba_keras(obs, model, m_pre_proc)
    
    except Exception as e:
        status = {'Error':[{'error_code':'model_prediction_failed'}, {'error_log':str(e)}]}
        ret['status'] = status
        return json.dumps(ret)
    
    out1= out.round(2)
    out2 = (100*out1).astype(int)
    formatted_output = post_process_observation(title, problem_description, resolution_description, out2, status)
    #formatted_output['delay']=timeit.default_timer() - start_time0
    #print("\n")
    #print(json.dumps(formatted_output))
    #print("\n")
    #return jsonify(formatted_output)
    return (formatted_output)

@app.route('/test')
def WelcomeToMyapp():
    return 'Welcome, to my app!'

#%% Start the port
port = os.getenv('PORT', '6000')
if __name__ == "__main__":
    app.run(host='0.0.0.0', port=int(port))

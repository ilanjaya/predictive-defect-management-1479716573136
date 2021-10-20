# -*- coding: utf-8 -*-
"""
@author: nilesh

Tasks: Completed
1. Write config file with dir structure
2. Read folder structure from config file
3. Create folder structure if not exist

Tasks: Pending
"""

from __future__ import print_function
import os
import sys
from configobj import ConfigObj


# Set Base Path: --------------------------------------------------------------
path_base = os.path.dirname(os.path.dirname(os.path.realpath(__file__))) 
os.chdir(path_base)
sys.path.append(path_base)


# Directory Structure: --------------------------------------------------------
dir_structure = {
    'fld_user_inputs' : 'a_user_inputs',
    'fld_model_meta_data' : 'b_model_meta_data',
    'fld_inp_data' : 'c_input_data',
    'fld_inp_data_pre_proc' : 'd_pre_processed_data',
    'fld_data_featur_eng' : 'e_feature_eng',
    'fld_insights' : 'f_insights',
    'fld_model_pipeline' : 'g_model_pipeline',
    'fld_model_param_tun' : 'h_model_param_tun',
    'fld_model_scoring' : 'i_model_scoring',
    'fld_model_result_summary' : 'j_model_result_summary',
    'fld_ensm_pipeline' : 'k_ensm_pipeline',
    'fld_ensm_param_tun' : 'l_ensm_param_tun',
    'fld_ensm_scoring' : 'm_ensm_scoring',
    'fld_ensm_result_summary' : 'n_ensm_result_summary',
    'fld_model_verification' : 'o_model_verification',
    'fld_unit_tests' : 'p_unit_tests',
    'fld_scripts' : 'q_scripts',
    'fld_docs' : 's_documentation',
    'fld_logs' : 'r_logs'
}


# Write Config File: ----------------------------------------------------------
config = ConfigObj()
config.filename = 'config.ini'
config['dir_structure'] = dir_structure
config.write()



# Read Config File: -----------------------------------------------------------
from q_scripts.a_class_func_dir import DirStructure
fld = DirStructure('config.ini')


# Create Directory Structure: -------------------------------------------------
for ver in fld.__dict__:
    fld.create_fld_if_not_exist(getattr(fld, ver))


# Run Code: -------------------------------------------------------------------
def main():
    pass

if __name__ == "__main__":
    main()
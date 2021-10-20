# -*- coding: utf-8 -*-
"""
@authors: Cognitive Development Team (Nilesh, Mandar, Abhishek, Gomathy, Rahul, Anil)
"""
from __future__ import print_function
import sys
import os
import requests
import time


# User Inputs -----------------------------------------------------------------
path_base = os.path.dirname(os.path.dirname(os.path.realpath(__file__)))

# Set Base Path----------------------------------------------------------------
os.chdir(path_base)
sys.path.append(path_base)
from q_scripts.a_class_func_dir import DirStructure

# Read Config File: -----------------------------------------------------------
fld = DirStructure('config.ini')

def test():
    # http://169.47.19.170:5004/CI_DRM_IDC_Target_noTP?title=new path order got kicked error code 999&problem_description=steps reproduce 1 initiate order new path 2 add wl configure 3 add dsl telco company fibe internet 15 10 configure offer offer nc targeted 9 m1 12 52 95 4 add telco company&resolution_description=there is not resolution for this test 
    orig_url = "http://169.47.19.170:6000/CI_DRM_IDC_Target_noTP" 
    r = requests.post(orig_url, 
                      data={'title': 'new path order got kicked error code 999', 
                            'problem_description': 'steps reproduce 1 initiate order new path 2 add wl configure 3 add dsl telco company fibe internet 15 10 configure offer offer nc targeted 9 m1 12 52 95 4 add telco company', 
                            'resolution_description': 'there is not resolution for this test'})
    print(r.status_code, r.reason)
    print(r.text[:300] + '...')
    r = requests.post("http://169.47.19.170:6000/CI_DRM_IDC_Target_noTP", 
                      data={'title': '', 
                            'problem_description': '', 
                            'resolution_description': ''})
    print(r.status_code, r.reason)
    print(r.text[:300] + '...')
    r = requests.post("http://169.47.19.170:6000/CI_DRM_IDC_Target_noTP",
                      data={'title': 'and', 
                            'problem_description': 'the', 
                            'resolution_description': 'is'})
    print(r.status_code, r.reason)
    print(r.text[:300] + '...')

    params = {}
    params['title'] = "the "
    params['problem_description'] = "and it is the important"
    params['resolution_description'] = "and it is the important "
    r = requests.post(orig_url, data=params)
    print (r.text)

    
def timing(f):
    def wrap(*args):
        time1 = time.time()
        ret = f(*args)
        time2 = time.time()
        print('%s function took %0.3f ms' % (f.__name__, (time2-time1)*1000.0))
        return ret
    return wrap
    
@timing
def test_timings():
    orig_url = "http://169.47.19.170:6000/CI_DRM_IDC_Target_noTP" 
    params = {}
    params['title'] = "Functional Requirement ID is missing"
    params['problem_description'] = "The last Functional Requirement List entry does not have a Functional Requirement ID."
    params['resolution_description'] = "Functional requirement ID is provided"
    r = requests.post(orig_url, data=params)
    print (r.text)


def test_local():
    orig_url = "http://localhost:6000/CI_DRM_IDC_Target_noTP" 
    params = {}
    params['title'] = "IESA_DEV_Renewals_Unable to save/transact the quote"
    params['problem_description'] = """Renewals: Unable to save the quote or renew in devbeta4 
                                    We are unable to do renew/save quote. Also, unable to recalculate price and export to excel  by unselecting some of the licenses. 
                                    Steps to reproduce:
                                    1) Open devbeta4
                                    Url:http://devbeta4.citrite.net/MyCitrix/EmployeeMyCitrix/Frameset.aspx
                                        2)Click on Samri and login.
                                        3)Go to quote work sheet page
                                        4)Click on renew or save quote
                                        
                                        Test data:
                                        TestData:
                                        Login: gplachn987
                                        Customer Information
                                        Org ID: 45441738
                                        
                                        Please find the attachment for more details
                                        Navy/HP Inc"""
    params['resolution_description'] = ""
    r = requests.post(orig_url, data=params)
    print (r.text) # Data #-- failed
    
    params['title'] = "Opp2Create: The Opp2Create Process is failing to create opportunities"
    params['problem_description'] = """Issue: Error Description: System.ServiceModel.Security.MessageSecurityException: The HTTP request is 
                                    unauthorized with client authentication scheme 'Anonymous'. The authentication header received from the server 
                                    was ''. ---> System.Net.WebException: The remote server returned an error: (401) Unauthorized. 
                                    
                                    Tried for APAC, EMEA, Americas(NA) Customers.
                                    
                                    Please see the attachment."""
    params['resolution_description'] = ""
    r = requests.post(orig_url, data=params)
    print (r.text) # Environment #-- pass



    
#%% Start the port
if __name__ == "__main__":
    if True:
        test()
        test_timings()
    else:
        test_local()
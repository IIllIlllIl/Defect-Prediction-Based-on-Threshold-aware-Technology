# -*- coding: utf-8 -*-
import glob

from imblearn.over_sampling import SMOTE
from imblearn.under_sampling import RandomUnderSampler
from imblearn.over_sampling import RandomOverSampler
from imblearn.combine import SMOTEENN


from DataReading import *
from Classifier import *
'''
Dataset Import
'''
ALL_FILE_PATH = ['C:/Users/15531/Desktop/threshold/test/train/'
                 ]
# load training file
train_file = []
for file in ALL_FILE_PATH:
    for filename in glob.glob(r'' + file + '*.csv'):
        train_file.append(filename)



'''
predict
'''

for tf in train_file:
    print(tf)
    X, Y = data_reading(tf)
    X_test, Y_test = data_reading((tf.replace('train', 'test')))

    bug_rate = Y.count(1.0)
    Y_len = len(Y)

    bug_rate = float(bug_rate) / float(Y_len)


    prob_computing(X=X, Y=Y, X_test=X_test, Y_test=Y_test, br=bug_rate, dataset_name=tf, alg='ORIGIN')


    # smote
    sm = SMOTE(random_state=42)
    try:
        X_smote, Y_smote = sm.fit_resample(X, Y)
    except:
        X_smote, Y_smote = X, Y
    prob_computing(X=X_smote, Y=Y_smote, X_test=X_test, Y_test=Y_test, br=bug_rate, dataset_name=tf, alg='SMOTE')

    # under sampling
    rus = RandomUnderSampler(random_state=42)
    try:
        X_us, Y_us = rus.fit_resample(X, Y)
    except:
        X_us, Y_us = X, Y
    prob_computing(X=X_us, Y=Y_us, X_test=X_test, Y_test=Y_test, br=bug_rate, dataset_name=tf, alg='UNDER-SAMPLING')

    # over sampling
    ros = RandomOverSampler(random_state=42)
    try:
        X_os, Y_os = ros.fit_resample(X, Y)
    except:
        X_os, Y_os = X, Y
    prob_computing(X=X_os, Y=Y_os, X_test=X_test, Y_test=Y_test, br=bug_rate, dataset_name=tf, alg='OVER-SAMPLING')

    # under over sampling
    sme = SMOTEENN(random_state=42)
    try:
        X_uo, Y_uo = sme.fit_resample(X, Y)
    except:
        X_uo, Y_uo = X, Y
    prob_computing(X=X_uo, Y=Y_uo, X_test=X_test, Y_test=Y_test, br=bug_rate, dataset_name=tf,
                   alg='UNDER-OVER-SAMPLING')


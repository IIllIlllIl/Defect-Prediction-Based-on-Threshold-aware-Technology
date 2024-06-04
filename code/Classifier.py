from sklearn.linear_model import LogisticRegression
from sklearn.ensemble import BaggingClassifier
from sklearn.ensemble import AdaBoostClassifier
from sklearn.naive_bayes import GaussianNB
from sklearn.neighbors import KNeighborsClassifier
from sklearn.svm import SVC
from sklearn.ensemble import RandomForestClassifier
from sklearn.tree import DecisionTreeClassifier

from ThresholdAwareTechnique import *
from ResultSaving import *


'''
using different classifiers to predict the defects
param:
    X               -> the metrics of the training data
    Y               -> the label of the training data
    X_test          -> the metrics of the test data
    Y_test          -> the label of the test data
    br              -> the buggy ratio
    dataset_name    -> the name of the dataset
    alg             -> the method of resampling  (ORIGIN/SMOTE/UNDER-SAMPLING/OVER-SAMPLING/UNDER-OVER-SAMPLING)
'''
def prob_computing(X, Y, X_test, Y_test, br, dataset_name, alg):
    # Logistic Regression
    try:
        clf = LogisticRegression(random_state=0).fit(X, Y)
        pred_prob = clf.predict_proba(X_test)[:, 1]
        threshold_pred_prob = clf.predict_proba(X)[:, 1]
    except:
        pred_prob = [0 for i in range(len(Y_test))]
        threshold_pred_prob = [0 for i in range(len(Y))]
        print("error")

    max_f1_threshold, max_mcc_threshold, max_gmean_threshold, max_gmeasure_threshold, min_d2h_threshold = theshold_edge(
        threshold_pred_prob, Y)
    result = cutoff_generator(pred_prob, br, max_f1_threshold, max_mcc_threshold, max_gmean_threshold,
                              max_gmeasure_threshold, min_d2h_threshold)
    result_saving(Y_test=Y_test, result=result, dataset_name=dataset_name, alg=alg, alg1='ORIGIN', alg2='LR')

    try:
        clf = BaggingClassifier(base_estimator=LogisticRegression(), n_estimators=10, random_state=0).fit(
            X, Y)
        pred_prob = clf.predict_proba(X_test)[:, 1]
        threshold_pred_prob = clf.predict_proba(X)[:, 1]
    except:
        pred_prob = [0 for i in range(len(Y_test))]
        threshold_pred_prob = [0 for i in range(len(Y))]
    max_f1_threshold, max_mcc_threshold, max_gmean_threshold, max_gmeasure_threshold, min_d2h_threshold = theshold_edge(
        threshold_pred_prob, Y)
    result = cutoff_generator(pred_prob, br, max_f1_threshold, max_mcc_threshold, max_gmean_threshold,
                              max_gmeasure_threshold, min_d2h_threshold)

    result_saving(Y_test=Y_test, result=result, dataset_name=dataset_name, alg=alg, alg1='BAGGING', alg2='LR')

    try:
        clf = AdaBoostClassifier(base_estimator=LogisticRegression(), n_estimators=10,
                                 random_state=0).fit(X, Y)
        pred_prob = clf.predict_proba(X_test)[:, 1]
        threshold_pred_prob = clf.predict_proba(X)[:, 1]
    except:
        pred_prob = [0 for i in range(len(Y_test))]
        threshold_pred_prob = [0 for i in range(len(Y))]
    max_f1_threshold, max_mcc_threshold, max_gmean_threshold, max_gmeasure_threshold, min_d2h_threshold = theshold_edge(
        threshold_pred_prob, Y)
    result = cutoff_generator(pred_prob, br, max_f1_threshold, max_mcc_threshold, max_gmean_threshold,
                              max_gmeasure_threshold, min_d2h_threshold)

    result_saving(Y_test=Y_test, result=result, dataset_name=dataset_name, alg=alg, alg1='BOOSTING', alg2='LR')

    # Naive Bayes
    try:
        clf = GaussianNB().fit(X, Y)
        pred_prob = clf.predict_proba(X_test)[:, 1]
        threshold_pred_prob = clf.predict_proba(X)[:, 1]
    except:
        pred_prob = [0 for i in range(len(Y_test))]
        threshold_pred_prob = [0 for i in range(len(Y))]
    max_f1_threshold, max_mcc_threshold, max_gmean_threshold, max_gmeasure_threshold, min_d2h_threshold = theshold_edge(
        threshold_pred_prob, Y)
    result = cutoff_generator(pred_prob, br, max_f1_threshold, max_mcc_threshold, max_gmean_threshold,
                              max_gmeasure_threshold, min_d2h_threshold)

    result_saving(Y_test=Y_test, result=result, dataset_name=dataset_name, alg=alg, alg1='ORIGIN', alg2='NB')

    try:
        clf = BaggingClassifier(base_estimator=GaussianNB(), n_estimators=10, random_state=0).fit(X, Y)
        pred_prob = clf.predict_proba(X_test)[:, 1]
        threshold_pred_prob = clf.predict_proba(X)[:, 1]
    except:
        pred_prob = [0 for i in range(len(Y_test))]
        threshold_pred_prob = [0 for i in range(len(Y))]
    max_f1_threshold, max_mcc_threshold, max_gmean_threshold, max_gmeasure_threshold, min_d2h_threshold = theshold_edge(
        threshold_pred_prob, Y)
    result = cutoff_generator(pred_prob, br, max_f1_threshold, max_mcc_threshold, max_gmean_threshold,
                              max_gmeasure_threshold, min_d2h_threshold)

    result_saving(Y_test=Y_test, result=result, dataset_name=dataset_name, alg=alg, alg1='BAGGING', alg2='NB')

    try:
        clf = AdaBoostClassifier(base_estimator=GaussianNB(), n_estimators=10, random_state=0).fit(X, Y)
        pred_prob = clf.predict_proba(X_test)[:, 1]
        threshold_pred_prob = clf.predict_proba(X)[:, 1]
    except:
        pred_prob = [0 for i in range(len(Y_test))]
        threshold_pred_prob = [0 for i in range(len(Y))]
    max_f1_threshold, max_mcc_threshold, max_gmean_threshold, max_gmeasure_threshold, min_d2h_threshold = theshold_edge(
        threshold_pred_prob, Y)
    result = cutoff_generator(pred_prob, br, max_f1_threshold, max_mcc_threshold, max_gmean_threshold,
                              max_gmeasure_threshold, min_d2h_threshold)

    result_saving(Y_test=Y_test, result=result, dataset_name=dataset_name, alg=alg, alg1='BOOSTING', alg2='NB')

    # Random Forest
    try:
        clf = RandomForestClassifier().fit(X, Y)
        pred_prob = clf.predict_proba(X_test)[:, 1]
        threshold_pred_prob = clf.predict_proba(X)[:, 1]
    except:
        pred_prob = [0 for i in range(len(Y_test))]
        threshold_pred_prob = [0 for i in range(len(Y))]
    max_f1_threshold, max_mcc_threshold, max_gmean_threshold, max_gmeasure_threshold, min_d2h_threshold = theshold_edge(
        threshold_pred_prob, Y)
    result = cutoff_generator(pred_prob, br, max_f1_threshold, max_mcc_threshold, max_gmean_threshold,
                              max_gmeasure_threshold, min_d2h_threshold)

    result_saving(Y_test=Y_test, result=result, dataset_name=dataset_name, alg=alg, alg1='ORIGIN', alg2='RF')

    try:
        clf = BaggingClassifier(base_estimator=RandomForestClassifier(), n_estimators=10, random_state=0).fit(X, Y)
        pred_prob = clf.predict_proba(X_test)[:, 1]
        threshold_pred_prob = clf.predict_proba(X)[:, 1]
    except:
        pred_prob = [0 for i in range(len(Y_test))]
        threshold_pred_prob = [0 for i in range(len(Y))]
    max_f1_threshold, max_mcc_threshold, max_gmean_threshold, max_gmeasure_threshold, min_d2h_threshold = theshold_edge(
        threshold_pred_prob, Y)
    result = cutoff_generator(pred_prob, br, max_f1_threshold, max_mcc_threshold, max_gmean_threshold,
                              max_gmeasure_threshold, min_d2h_threshold)

    result_saving(Y_test=Y_test, result=result, dataset_name=dataset_name, alg=alg, alg1='BAGGING', alg2='RF')

    try:
        clf = AdaBoostClassifier(base_estimator=RandomForestClassifier(), n_estimators=10, random_state=0).fit(X, Y)
        pred_prob = clf.predict_proba(X_test)[:, 1]
        threshold_pred_prob = clf.predict_proba(X)[:, 1]
    except:
        pred_prob = [0 for i in range(len(Y_test))]
        threshold_pred_prob = [0 for i in range(len(Y))]
    max_f1_threshold, max_mcc_threshold, max_gmean_threshold, max_gmeasure_threshold, min_d2h_threshold = theshold_edge(
        threshold_pred_prob, Y)
    result = cutoff_generator(pred_prob, br, max_f1_threshold, max_mcc_threshold, max_gmean_threshold,
                              max_gmeasure_threshold, min_d2h_threshold)
    result_saving(Y_test=Y_test, result=result, dataset_name=dataset_name, alg=alg, alg1='BOOSTING', alg2='RF')

    # Decision Tree
    try:
        clf = DecisionTreeClassifier().fit(X, Y)
        pred_prob = clf.predict_proba(X_test)[:, 1]
        threshold_pred_prob = clf.predict_proba(X)[:, 1]
    except:
        pred_prob = [0 for i in range(len(Y_test))]
        threshold_pred_prob = [0 for i in range(len(Y))]
    max_f1_threshold, max_mcc_threshold, max_gmean_threshold, max_gmeasure_threshold, min_d2h_threshold = theshold_edge(
        threshold_pred_prob, Y)
    result = cutoff_generator(pred_prob, br, max_f1_threshold, max_mcc_threshold, max_gmean_threshold,
                              max_gmeasure_threshold, min_d2h_threshold)
    result_saving(Y_test=Y_test, result=result, dataset_name=dataset_name, alg=alg, alg1='ORIGIN', alg2='DT')

    try:
        clf = BaggingClassifier(base_estimator=DecisionTreeClassifier(), n_estimators=10, random_state=0).fit(X, Y)
        pred_prob = clf.predict_proba(X_test)[:, 1]
        threshold_pred_prob = clf.predict_proba(X)[:, 1]
    except:
        pred_prob = [0 for i in range(len(Y_test))]
        threshold_pred_prob = [0 for i in range(len(Y))]
    max_f1_threshold, max_mcc_threshold, max_gmean_threshold, max_gmeasure_threshold, min_d2h_threshold = theshold_edge(
        threshold_pred_prob, Y)
    result = cutoff_generator(pred_prob, br, max_f1_threshold, max_mcc_threshold, max_gmean_threshold,
                              max_gmeasure_threshold, min_d2h_threshold)
    result_saving(Y_test=Y_test, result=result, dataset_name=dataset_name, alg=alg, alg1='BAGGING', alg2='DT')

    try:
        clf = AdaBoostClassifier(base_estimator=DecisionTreeClassifier(), n_estimators=10, random_state=0).fit(X, Y)
        pred_prob = clf.predict_proba(X_test)[:, 1]
        threshold_pred_prob = clf.predict_proba(X)[:, 1]
    except:
        pred_prob = [0 for i in range(len(Y_test))]
        threshold_pred_prob = [0 for i in range(len(Y))]
    max_f1_threshold, max_mcc_threshold, max_gmean_threshold, max_gmeasure_threshold, min_d2h_threshold = theshold_edge(
        threshold_pred_prob, Y)
    result = cutoff_generator(pred_prob, br, max_f1_threshold, max_mcc_threshold, max_gmean_threshold,
                              max_gmeasure_threshold, min_d2h_threshold)
    result_saving(Y_test=Y_test, result=result, dataset_name=dataset_name, alg=alg, alg1='BOOSTING', alg2='DT')

    # SVM
    try:
        clf = SVC(probability=True).fit(X, Y)
        pred_prob = clf.predict_proba(X_test)[:, 1]
        threshold_pred_prob = clf.predict_proba(X)[:, 1]
    except:
        pred_prob = [0 for i in range(len(Y_test))]
        threshold_pred_prob = [0 for i in range(len(Y))]
    max_f1_threshold, max_mcc_threshold, max_gmean_threshold, max_gmeasure_threshold, min_d2h_threshold = theshold_edge(
        threshold_pred_prob, Y)
    result = cutoff_generator(pred_prob, br, max_f1_threshold, max_mcc_threshold, max_gmean_threshold,
                              max_gmeasure_threshold, min_d2h_threshold)
    result_saving(Y_test=Y_test, result=result, dataset_name=dataset_name, alg=alg, alg1='ORIGIN', alg2='SVM')

    try:
        clf = BaggingClassifier(base_estimator=SVC(probability=True), n_estimators=10, random_state=0).fit(X, Y)
        pred_prob = clf.predict_proba(X_test)[:, 1]
        threshold_pred_prob = clf.predict_proba(X)[:, 1]
    except:
        pred_prob = [0 for i in range(len(Y_test))]
        threshold_pred_prob = [0 for i in range(len(Y))]
    max_f1_threshold, max_mcc_threshold, max_gmean_threshold, max_gmeasure_threshold, min_d2h_threshold = theshold_edge(
        threshold_pred_prob, Y)
    result = cutoff_generator(pred_prob, br, max_f1_threshold, max_mcc_threshold, max_gmean_threshold,
                              max_gmeasure_threshold, min_d2h_threshold)
    result_saving(Y_test=Y_test, result=result, dataset_name=dataset_name, alg=alg, alg1='BAGGING', alg2='SVM')

    try:
        clf = AdaBoostClassifier(base_estimator=SVC(probability=True), n_estimators=10, random_state=0).fit(X, Y)
        pred_prob = clf.predict_proba(X_test)[:, 1]
        threshold_pred_prob = clf.predict_proba(X)[:, 1]
    except:
        pred_prob = [0 for i in range(len(Y_test))]
        threshold_pred_prob = [0 for i in range(len(Y))]
    max_f1_threshold, max_mcc_threshold, max_gmean_threshold, max_gmeasure_threshold, min_d2h_threshold = theshold_edge(
        threshold_pred_prob, Y)
    result = cutoff_generator(pred_prob, br, max_f1_threshold, max_mcc_threshold, max_gmean_threshold,
                              max_gmeasure_threshold, min_d2h_threshold)
    result_saving(Y_test=Y_test, result=result, dataset_name=dataset_name, alg=alg, alg1='BOOSTING', alg2='SVM')

    # KNN
    # try:
    #     clf = KNeighborsClassifier().fit(X, Y)
    #     pred_prob = clf.predict_proba(X_test)[:, 1]
    #     threshold_pred_prob = clf.predict_proba(X)[:, 1]
    # except:
    #     pred_prob = [0 for i in range(len(Y_test))]
    #     threshold_pred_prob = [0 for i in range(len(Y))]
    # max_f1_threshold, max_mcc_threshold, max_gmean_threshold, max_gmeasure_threshold, min_d2h_threshold = theshold_edge(
    #     threshold_pred_prob, Y)
    # result = cutoff_generator(pred_prob, br, max_f1_threshold, max_mcc_threshold, max_gmean_threshold,
    #                           max_gmeasure_threshold, min_d2h_threshold)
    # result_saving(Y_test=Y_test, result=result, dataset_name=dataset_name, alg=alg, alg1='ORIGIN', alg2='KNN')
    #
    # try:
    #     clf = BaggingClassifier(base_estimator=KNeighborsClassifier(), n_estimators=10, random_state=0).fit(X, Y)
    #     pred_prob = clf.predict_proba(X_test)[:, 1]
    #     threshold_pred_prob = clf.predict_proba(X)[:, 1]
    # except:
    #     pred_prob = [0 for i in range(len(Y_test))]
    #     threshold_pred_prob = [0 for i in range(len(Y))]
    # max_f1_threshold, max_mcc_threshold, max_gmean_threshold, max_gmeasure_threshold, min_d2h_threshold = theshold_edge(
    #     threshold_pred_prob, Y)
    # result = cutoff_generator(pred_prob, br, max_f1_threshold, max_mcc_threshold, max_gmean_threshold,
    #                           max_gmeasure_threshold, min_d2h_threshold)
    # result_saving(Y_test=Y_test, result=result, dataset_name=dataset_name, alg=alg, alg1='BAGGING', alg2='KNN')

    # clf = AdaBoostClassifier(base_estimator=KNeighborsClassifier(), n_estimators=10, random_state=0).fit(X, Y)
    # pred_prob = clf.predict_proba(X_test)[:, 1]
    # result = cutoff_generator(pred=pred_prob, br=br)
    # result_saving(Y_test=Y_test, result=result, dataset_name=dataset_name, alg=alg, alg1='BOOSTING', alg2='KNN')



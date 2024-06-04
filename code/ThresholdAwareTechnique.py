import math
import copy

from imblearn.metrics import geometric_mean_score
from sklearn import metrics


'''
Use threshold-aware technology to give prediction results based on predicted values
param:
    pred                    -> the prediction value
    br                      -> buggt ratio
    max_f1_threshold        -> the threshold that maximizes f1
    max_mcc_threshold       -> the threshold that maximizes mcc
    max_gmean_threshold     -> the threshold that maximizes gmean
    max_gmeasure_threshold  -> the threshold that maximizes gmeasure
    min_d2h_threshold       -> the threshold that minimizes d2h
return:
    result                  -> the prediction result
'''
def cutoff_generator(pred, br, max_f1_threshold, max_mcc_threshold, max_gmean_threshold, max_gmeasure_threshold,
                     min_d2h_threshold):
    result = []
    # Numerical threshold
    result.append([1 if x > 0.5 else 0 for x in pred])
    result.append([1 if x > br else 0 for x in pred])
    result.append([1 if x > ((0.5 + br) / 2) else 0 for x in pred])
    result.append([1 if x > math.sqrt(0.5 * br) else 0 for x in pred])

    # Rank threshold
    cp_pred = copy.copy(pred)
    cp_pred = list(cp_pred)
    cp_pred.sort(reverse=True)
    a = cp_pred[int((0.5 * len(cp_pred)))]
    b = cp_pred[int((br * len(cp_pred)))]
    temp = int((((0.5 + br) / 2) * len(cp_pred)))
    tt = (0.5 + br) / 2
    temp1 = int((math.sqrt(0.5 * br) * len(cp_pred)))
    tt1 = math.sqrt(0.5 * br)
    c = cp_pred[int((((0.5 + br) / 2) * len(cp_pred)))]
    d = cp_pred[int((math.sqrt(0.5 * br) * len(cp_pred)))]
    result.append([1 if x > a else 0 for x in pred])
    result.append([1 if x > b else 0 for x in pred])
    result.append([1 if x > c else 0 for x in pred])
    result.append([1 if x > d else 0 for x in pred])

    # Condition threshold
    max_f1_threshold, max_mcc_threshold, max_gmean_threshold, max_gmeasure_threshold, min_d2h_threshold
    result.append([1 if x > max_f1_threshold else 0 for x in pred])
    result.append([1 if x > max_mcc_threshold else 0 for x in pred])
    result.append([1 if x > max_gmean_threshold else 0 for x in pred])
    result.append([1 if x > max_gmeasure_threshold else 0 for x in pred])
    result.append([1 if x > min_d2h_threshold else 0 for x in pred])
    return result


'''
Calculate the condition threshold
param:
    Y                   -> the label of the training set
    threshold_pred_prob -> the prediction value of the training set
return:
    max_f1_threshold        -> the threshold that maximizes f1
    max_mcc_threshold       -> the threshold that maximizes mcc
    max_gmean_threshold     -> the threshold that maximizes gmean
    max_gmeasure_threshold  -> the threshold that maximizes gmeasure
    min_d2h_threshold       -> the threshold that minimizes d2h
'''
def theshold_edge(threshold_pred_prob, Y):
    threshold = 0
    max_f1 = 0
    max_f1_threshold = 0
    max_gmean = 0
    max_gmean_threshold = 0
    max_gmeasure = 0
    max_gmeasure_threshold = 0
    max_mcc_threshold = 0
    min_d2h_threshold = 0
    max_mcc = -1
    min_d2h = 999
    while threshold < 0.5:
        threshold += 0.01
        r = [1 if x > threshold else 0 for x in threshold_pred_prob]
        tn, fp, fn, tp = metrics.confusion_matrix(Y, r, labels=[0, 1]).ravel()
        f1 = metrics.f1_score(Y, r)
        mcc = metrics.matthews_corrcoef(Y, r)
        gmean = geometric_mean_score(Y, r, average='weighted')
        rc = metrics.recall_score(Y, r, average='binary')
        gmeasure = (2 * rc * (1 - (fp / (fp + tn)))) / (rc + (1 - (fp / (fp + tn))))
        d2h = math.sqrt(math.pow(1 - rc, 2) + math.pow(0 - (fp / (tn + fp)), 2)) / math.sqrt(2)
        if f1 > max_f1:
            max_f1 = f1
            max_f1_threshold = threshold
        if mcc > max_mcc:
            max_mcc = mcc
            max_mcc_threshold = threshold
        if gmean > max_gmean:
            max_gmean = gmean
            max_gmean_threshold = threshold
        if gmeasure > max_gmeasure:
            max_gmeasure = gmeasure
            max_gmeasure_threshold = threshold
        if d2h > min_d2h:
            min_d2h = d2h
            min_d2h_threshold = threshold
    return max_f1_threshold, max_mcc_threshold, max_gmean_threshold, max_gmeasure_threshold, min_d2h_threshold
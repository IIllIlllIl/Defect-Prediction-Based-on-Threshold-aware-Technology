
import math

from imblearn.metrics import geometric_mean_score
from sklearn import metrics

'''
Result saving
param: 
    Y_test       -> the label of the test set
    result       -> the prediction result of the test set
    dataset_name -> the file name of the dataset
    alg1         -> the method of resampling  (ORIGIN/SMOTE/UNDER-SAMPLING/OVER-SAMPLING/UNDER-OVER-SAMPLING)
    alg2         -> the method of ensemble learning   (ORIGIN/BAGGING/BOOSTING)
    alg3         -> the classifier  (LR/NB/RF/DT/SVM)
    
 
'''

def result_saving(Y_test, result, dataset_name, alg, alg1, alg2):
    # filename    delete _x_A/B in name
    dataset_name_saving = dataset_name.split('/')[-1].split('_')
    dataset_name_saving.pop(-1)
    dataset_name_saving.pop(-1)
    dataset_name_saving = " ".join(dataset_name_saving)

    # repo
    repo_name_saving = dataset_name.split('/')[-3]
    ind = 0
    for r in result:
        # prediction value saving
        midname = dataset_name.split('/')[-1].split('.csv')[0]
        midname = repo_name_saving + "_" + midname + "_" + alg + "_" + alg1 + "_" + alg2 + "_" + str(ind) + ".csv"

        # result to csv
        tn, fp, fn, tp = metrics.confusion_matrix(Y_test, r, labels=[0, 1]).ravel()
        rc = metrics.recall_score(Y_test, r, average='binary')
        pr = metrics.precision_score(Y_test, r, average='binary')
        auc = metrics.roc_auc_score(Y_test, r)
        f1 = metrics.f1_score(Y_test, r)
        mcc = metrics.matthews_corrcoef(Y_test, r)
        # brier = metrics.brier_score_loss(Y_test, r)
        # accuracy = metrics.accuracy_score(Y_test, r)
        gmean = geometric_mean_score(Y_test, r, average='weighted')
        gmeasure = (2 * rc * (1 - (fp / (fp + tn)))) / (rc + (1 - (fp / (fp + tn))))
        d2h = math.sqrt(math.pow(1 - rc, 2) + math.pow(0 - (fp / (tn + fp)), 2)) / math.sqrt(2)


        # save file
        new_file = r'C:\\Users\\15531\\Desktop\\threshold\\test\\new.csv'
        f = open(new_file, 'a+')
        f.write(
            repo_name_saving + "," + dataset_name_saving + "," + dataset_name + "," + alg + "," + alg1 + "," + alg2 + ","
            + str(ind) + "," + str(rc) + "," + str(pr) + "," + str(f1) + "," + str(mcc) + "," + str(auc) + ","
            + str(gmean) + "," + str(gmeasure) + "," + str(d2h) + "\n")
        f.close()
        ind += 1
# Import the packages
import pandas as pd
import os
import numpy as np
import xgboost as xgb
from xgboost.sklearn import XGBClassifier
from sklearn import cross_validation, metrics 
from sklearn.grid_search import GridSearchCV
from sklearn.preprocessing import LabelEncoder
le = LabelEncoder()
os.chdir("C:/My Files/Project/networking")
import matplotlib.pylab as plt
%matplotlib inline
from matplotlib.pylab import rcParams
rcParams['figure.figsize'] = 12, 4

# Train test split

data = pd.read_csv("FinalDataset89.csv")
target = 'Churn'
predictors = [x for x in data.columns if x not in [target]]
for i in data.columns:
    if data[i].dtype == 'object':
        le.fit(data[i])
        data[i] = le.transform(data[i])

A0 = data[data.Churn == 0]
A1 = data[data.Churn == 1]
#Down sampling the data
np.random.seed(22)
s = np.random.rand(len(A0)) < 0.10
t = np.random.rand(len(A1)) < 0.80
A0_train = A0[s]
A0_test = A0[~s]
A1_train = A1[t]
A1_test = A1[~t]

dtrain = A0_train.append(A1_train)
print(train.shape)
dtest = A0_test.append(A1_test)
print(test.shape)

# Model building

def modelfit(alg, dtrain, predictors,useTrainCV=True, cv_folds=5, early_stopping_rounds=50):
    if useTrainCV:
        
        xgb_param = alg.get_xgb_params()
        xgtrain = xgb.DMatrix(dtrain[predictors].values, label=dtrain[target].values)
        cvresult = xgb.cv(xgb_param, xgtrain, num_boost_round=alg.get_params()['n_estimators'], nfold=cv_folds,
            metrics={'error'}, early_stopping_rounds=early_stopping_rounds)
        alg.set_params(n_estimators=cvresult.shape[0])
    
    #Fit the algorithm on the data
    alg.fit(dtrain[predictors], dtrain['Churn'],eval_metric='auc')
        
    #Predict training set:
    dtrain_predictions = alg.predict(dtrain[predictors])
    dtrain_predprob = alg.predict_proba(dtrain[predictors])[:,1]
    dtest_predictions = alg.predict(dtest[predictors])
    dtest_predprob = alg.predict_proba(dtest[predictors])[:,1]
        
    #Print model report:
    print("\nTrainingModel Report")
    print("Training Accuracy : {}".format(metrics.accuracy_score(dtrain['Churn'].values, dtrain_predictions)))
    print("Training AUC Score (Train): {}".format(metrics.roc_auc_score(dtrain['Churn'], dtrain_predprob)))
    
    print("\nTesting Model Report")
    print("Testing Accuracy : {}".format(metrics.accuracy_score(dtest['Churn'].values, dtest_predictions)))
    print("Testing AUC Score (Train): {}".format(metrics.roc_auc_score(dtest['Churn'], dtest_predprob)))
    
    print(metrics.confusion_matrix(dtest['Churn'].values, dtest_predictions))
    print(metrics.classification_report(dtest['Churn'].values, dtest_predictions))
                    


#Choose all predictors except target & IDcols
predictors = [x for x in data.columns if x not in [target]]
xgb1 = XGBClassifier(
 learning_rate =0.3,
 n_estimators=1000,
 max_depth=8,
 min_child_weight=1,
 gamma=0,
 subsample=0.9,
 colsample_bytree=0.9,
 objective= 'binary:logistic',
 nthread=4,
 scale_pos_weight=1,
 seed=27)
modelfit(xgb1, data, predictors)

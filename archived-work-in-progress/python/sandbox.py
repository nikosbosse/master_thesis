import pandas as pd
import numpy as np
import os
os.chdir("/home/nikos/Desktop/data/Google Drive/Uni/Statistik/Masterarbeit Statistik/python/")

import matplotlib.pyplot as plt
import functions_masterthesis as f
import xgboost as xgb
from sklearn.model_selection import train_test_split
from sklearn.model_selection import TimeSeriesSplit
from sklearn.metrics import accuracy_score
plt.style.use('fivethirtyeight')

from xgboost import plot_importance, plot_tree

import importlib
# from fbprophet import Prophet
# importlib.reload(f)

# inc = f.my_load_data()
# inc = pd.to_numeric(inc["total_cases_change"])

# incidences = inc

# X_train, X_test, y_train, y_test = f.my_test_train_data(inc)
# reg = xgb.XGBRegressor(n_estimators=1000)
# a = reg.fit(X_train, y_train,
#         eval_set=[(X_train, y_train), (X_test, y_test)],
#         early_stopping_rounds=50,
#        verbose=True)


# #_ = xgb.plot_importance(reg, height=0.9)

# y_pred = reg.predict(X_test)

# plt.plot(y_pred)
# plt.show()




pjme = pd.read_csv('../data/PJME_hourly.csv', index_col=[0], parse_dates=[0])

color_pal = ["#F8766D", "#D39200", "#93AA00", "#00BA38", "#00C19F", "#00B9E3", "#619CFF", "#DB72FB"]
_ = pjme.plot(style='.', figsize=(15,5), color=color_pal[0], title='PJM East')
# plt.show()


split_date = '01-Jan-2015'
pjme_train = pjme.loc[pjme.index <= split_date].copy()
pjme_test = pjme.loc[pjme.index > split_date].copy()

_ = pjme_test \
    .rename(columns={'PJME_MW': 'TEST SET'}) \
    .join(pjme_train.rename(columns={'PJME_MW': 'TRAINING SET'}), how='outer') \
    .plot(figsize=(15,5), title='PJM East', style='.')



def create_features(df, label=None):
    """
    Creates time series features from datetime index
    """
    df['date'] = df.index
    df['hour'] = df['date'].dt.hour
    df['dayofweek'] = df['date'].dt.dayofweek
    df['quarter'] = df['date'].dt.quarter
    df['month'] = df['date'].dt.month
    df['year'] = df['date'].dt.year
    df['dayofyear'] = df['date'].dt.dayofyear
    df['dayofmonth'] = df['date'].dt.day
    df['weekofyear'] = df['date'].dt.weekofyear
    
    X = df[['hour','dayofweek','quarter','month','year',
           'dayofyear','dayofmonth','weekofyear']]
    if label:
        y = df[label]
        return X, y
    return X




X_train, y_train = create_features(pjme_train, label='PJME_MW')
X_test, y_test = create_features(pjme_test, label='PJME_MW')



reg = xgb.XGBRegressor(n_estimators=1000)
reg.fit(X_train, y_train,
        eval_set=[(X_train, y_train), (X_test, y_test)],
        early_stopping_rounds=50,
       verbose=False) # Change verbose to True if you want to see it train

_ = plot_importance(reg, height=0.9)

pjme_test['MW_Prediction'] = reg.predict(X_test)
pjme_all = pd.concat([pjme_test, pjme_train], sort=False)

_ = pjme_all[['PJME_MW','MW_Prediction']].plot(figsize=(15, 5))

plt.show()



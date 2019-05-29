import pandas as pd

result = pd.read_csv("result.csv")

# Run linear regression
from sklearn import linear_model
from sklearn.metrics import mean_squared_error, r2_score

x_lst = result.columns.tolist()
x_lst.remove('tx_date')
x_lst.remove('Market Cap($)')

#Define function that saves linear regression result as dataframe
def make_result(x_lst, target):
    coef_lst = []
    score_lst = []
    for x in x_lst:
        X = result[x].values
        y = result[target].values
        X = X.reshape(len(X), 1)
        y = y.reshape(len(y), 1)
        regr = linear_model.LinearRegression()
        regr.fit(X, y)
        coef_lst.append(regr.coef_[0][0])
        score_lst.append(regr.score(X, y))
    return coef_lst, score_lst
 
#Linear regression per each variable
coef_lst, score_lst = make_result(x_lst, 'Market Cap($)')

#Save linear regression result as csv
empty_lst = [0] * len(x_lst)
final = pd.DataFrame({'Coef' : empty_lst, 'Score' : empty_lst})
final['x_lst'] = x_lst
final.Coef = coef_lst
final.Score = score_lst
final = final.set_index('x_lst')
final.to_csv("final.csv")


#Multiple Regression with top3 highest r scored variables 
import statsmodels.api as sm

X = sm.add_constant(X)
model = sm.OLS(result['Market Cap($)'], result[['DAU', 'tx_count', 'Volume($)']]).fit()

predictions = model.predict()
print_model = model.summary()
print(print_model)

#PCA linear regression using PLS
top3 = result[['tx_date','DAU', 'tx_count', 'Volume($)']]
top3 = top3.set_index('tx_date')
top3.head()

from sklearn.decomposition import PCA

pca = PCA(n_components=1)
model_pca = pca.fit(top3.values)
model_pca.transform(top3.values)

X = top3
y = result['Market Cap($)']

from sklearn.cross_decomposition import PLSRegression

pls = PLSRegression(n_components=1)
pls.fit(X, y)

pls.score(X, y)
#Score as 0.832262975413609

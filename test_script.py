from sklearn.datasets import load_iris
from sklearn.ensemble import AdaBoostClassifier
from sklearn.model_selection import cross_val_score

data = load_iris()
X = data.data()
y = data.target()

clf_ada = AdaBoostClassifier()
clf_ada.fit(X, y)
clf_ada.feature_importances_

cv_scores = cross_val_score(clf_ada, X, y, cv=5)
print("hello changes")

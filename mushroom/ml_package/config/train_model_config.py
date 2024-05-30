from sklearn.tree import DecisionTreeClassifier

DATA_NAME = 'agaricus-lepiota'
TARGET = 'poisonous'
RANDOM_STATE = 0

MODELS = {
DATA_NAME:
        {

            'model': DecisionTreeClassifier,
            'params': {
                'max_depth': None,
                'min_samples_split': 2,
                'min_samples_leaf': 1

            }
        }
}

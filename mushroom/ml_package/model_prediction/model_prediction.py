import pandas as pd
import numpy as np
from sklearn.preprocessing import LabelEncoder
import matplotlib.pyplot as plt
from sklearn import tree
from model_training.train_models import encode_categorical_fields
from config.prediction_config import *



def get_prediction(prep_new_data_dict: dict,
                   model_dict: dict, cat_encode_dict: dict) -> tuple:
    """
    get dictionary of confusion matrix and prediction proba
    :param prep_new_data_dict: the pre-processed new data or test data dictionary
    :param model_dict: dictionary of model
    :param cat_encode_dict: dictionary of categorical encoding
    :return: dictionary of the predictions and reporting data
    """
    prediction_dict = {}
    for prep_data_dict_key, prep_data_dict_value in prep_new_data_dict.items():
        class_name = prep_data_dict_key

        data = prep_new_data_dict[class_name]
        model = model_dict[class_name]
        cat_encode = cat_encode_dict[class_name]

        class_label = cat_encode[TARGET]
        le = LabelEncoder()
        le.fit(class_label)

        data = encode_categorical_fields(data, cat_encode)
        df = data.copy()
        df = df.drop([TARGET], axis=1)
        predict = model.predict(df)
        predict = le.inverse_transform(predict)
        df_predict = pd.DataFrame({class_name: predict})
        actual = le.inverse_transform(data[TARGET])
        y_true = pd.Series(actual, name="Actual")
        y_pred = pd.Series(predict, name='Predicted')

        df_crosstab = pd.crosstab(y_true, y_pred)

        predict_proba = model.predict_proba(df)
        df_predict_proba = pd.DataFrame(predict_proba)
        df_predict_proba.columns = le.inverse_transform(df_predict_proba.columns)

        df_pred_out = pd.concat([df_predict, df_predict_proba], axis=1)
        prediction_dict[class_name] = df_pred_out

        #Graph
        fig = plt.figure(figsize=(25,20))
        _ = tree.plot_tree(model, feature_names=list(df.columns),
                   class_names=np.unique(list(actual)), filled=True, rounded=True)
        fig.savefig("decision_tree.png")

    return prediction_dict, df_crosstab

from data_loader.loader import *
from data_preprocessing.pre_processing import *
from model_prediction.model_prediction import *
from model_training.train_models import *

def get_test_data(df_dict: DataFrame):
    """
    get DataFrame of the test data
    :param df_dict: the dict of the whole data
    :return: dictionary of the DataFrame of the cobination of X_test and y_test
    """
    df_test_dict = {}
    df = df_dict[DATA_NAME]
    df = df.copy()
    cat_encode_dict = categorical_encoding_loader_scrip(df)

    df = encode_categorical_fields(df, cat_encode_dict)
    X_train, X_test, y_train, y_test = divide_train_test(df, target=TARGET)
    y_test = y_test.to_frame()

    df_test = pd.concat([X_test, y_test], axis=1)
    df_test_dict[DATA_NAME] = df_test

    return df_test_dict


def make_prediction(df_dict: dict):

    prep_train_data_dict = preprocess_data(data_dict=df_dict)
    print('pre-processing completed')

    model_dict, cat_encode_dict = train_model(prep_train_data_dict)
    print('Done training model')

    df_test = get_test_data(df_dict)
    print('Done loading new data')

    # Test data
    prep_test_data_dict = preprocess_data(data_dict=df_test)
    print('Done pre-processing new data')

    pred_dict, confusion_matrix = get_prediction(prep_test_data_dict, model_dict, cat_encode_dict)
    print('Done scoring new data')


    print(confusion_matrix)
    print(pred_dict)





import time

if __name__ == "__main__":
    start_time = time.process_time()

    df_dict = load_data()
    print('Loading completed')
    make_prediction(df_dict)



    total_time = time.process_time() - start_time
    print(total_time)

import pandas as pd
from config.loader_config import *


def _validate_and_get_file_name(path: str) -> list:
    """
    To get filename and ensure that folder is not empty.
    :param path: The path to the required dataset(s) on the local folder.
    :return: List of all files in the path.
    """
    files = os.listdir(path)
    if len(files) < 1:
        raise FileNotFoundError(f'No file detected in {path}')
        if path is None:
            raise ValueError(f'File path must be supplied.')

    return files

def load_data() -> dict:
    read_file_dict = {}
    """
    This Function loads local .data file to a pandas dataframe
    :return: Dictionary containing Pandas dataframe
    """
    files = _validate_and_get_file_name(DATASET_SOURCE_PATH)
    for file in files:
        f = file.split('.')
        if f[1] == 'data':
            df = pd.read_csv(os.path.join(DATASET_SOURCE_PATH, file), names=NAMES)
            read_file_dict[f[0]] = df

    return read_file_dict

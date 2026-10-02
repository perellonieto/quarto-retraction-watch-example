import pandas
import os

DEFAULT_DATAFOLDER = "data/"
DEFAULT_CSVFILE = "retraction_watch.csv"
DEFAULT_ENCODING = "iso-8859-1"


def read_dataset(datafolder=DEFAULT_DATAFOLDER, csvfile=DEFAULT_CSVFILE,
                 encoding=DEFAULT_ENCODING):
    df = pandas.read_csv(os.path.join(datafolder, csvfile), encoding=encoding)
    return df
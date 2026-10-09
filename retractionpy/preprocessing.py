import pandas


def clean_dataset(df):
    """
    Clean the dataset by dropping rows with missing values in 'RetractionDate' and 'OriginalPaperDate'.
    
    Parameters:
    df (DataFrame): The input DataFrame to be cleaned.
    
    Returns:
    DataFrame: The cleaned DataFrame with rows containing NaN in specified columns removed.
    """
    df.dropna(subset=['RetractionDate', 'OriginalPaperDate'], inplace=True)
    
    df['RetractionDate'] = pandas.to_datetime(df['RetractionDate'], errors='coerce', format='mixed')
    df['OriginalPaperDate'] = pandas.to_datetime(df['OriginalPaperDate'], errors='coerce', format='mixed')
    df["Record ID"] = df["Record ID"].astype(int)
    df["RetractionID"] = df["Record ID"]

    df["Involved countries"] = df.Country
    df["Country"] = df.Country.str.split(";")
    df = df.explode("Country", ignore_index=True)
    return df
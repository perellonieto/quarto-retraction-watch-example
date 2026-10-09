def plot_scatter_retraction_dates(df):
    """
    Plot a scatter plot of OriginalPaperDate vs RetractionDate.

    Parameters
    ----------
    df : pandas.DataFrame
        DataFrame containing 'OriginalPaperDate' and 'RetractionDate' columns.
    """

    fig = df.plot.scatter(x='OriginalPaperDate', y='RetractionDate', color='Country',
                          hover_data=["Subject", "Journal", "Publisher", "RetractionNature", "Reason"])
    return fig

#'Record ID', 'Title', 'Subject', 'Institution', 'Journal', 'Publisher',
#       'Country', 'Author', 'URLS', 'ArticleType', 'RetractionDate',
#       'RetractionDOI', 'RetractionPubMedID', 'OriginalPaperDate',
#       'OriginalPaperDOI', 'OriginalPaperPubMedID', 'RetractionNature',
#       'Reason', 'Paywalled', 'Notes', 'Unnamed: 20'
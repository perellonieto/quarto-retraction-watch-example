def plot_scatter_retraction_dates(df):
    """
    Plot a scatter plot of OriginalPaperDate vs RetractionDate.

    Parameters
    ----------
    df : pandas.DataFrame
        DataFrame containing 'OriginalPaperDate' and 'RetractionDate' columns.
    """

    fig = df.plot.scatter(x='OriginalPaperDate', y='RetractionDate', color='Country')
    return fig
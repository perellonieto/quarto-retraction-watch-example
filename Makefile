download-data:
	mkdir -p data
	curl  --output-dir data -O https://gitlab.com/crossref/data/-/raw/main/retraction_watch.csv

install-dependencies:
	Rscript install_dependencies.R

preview:
	quarto preview

render:
	quarto render

publish:
	quarto publish gh-pages

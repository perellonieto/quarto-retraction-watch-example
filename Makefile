.PHONY: download-data install-dependencies test preview render publish

download-data:
	mkdir -p data
	curl  --output-dir data -O https://gitlab.com/crossref/data/-/raw/main/retraction_watch.csv

install-dependencies:
	Rscript install_dependencies.R
	Rscript -e 'devtools::document("retractionwatch"); devtools::install("retractionwatch", upgrade=FALSE, quiet=TRUE)'

test:
	Rscript -e 'devtools::test("retractionwatch")'
	Rscript -e 'print(covr::package_coverage("retractionwatch"))'

preview:
	quarto preview

render:
	quarto render

publish:
	quarto publish gh-pages

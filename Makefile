download-data:
	mkdir -p retraction-watch-data
	curl  --time-cond "Aug 13, 2026" --output-dir retraction-watch-data -O https://gitlab.com/crossref/retraction-watch-data/-/raw/main/retraction_watch.csv

preview:
	quarto preview

render:
	quarto render

publish:
	quarto publish gh-pages
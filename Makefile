.PHONY: download-data install-dependencies test generate-documentation preview render publish run-comparison ci-local

download-data:
	mkdir -p data
	curl -fL -o data/retraction_watch.csv https://api.labs.crossref.org/data/retractionwatch

install-dependencies:
	Rscript install_dependencies.R
	Rscript -e 'devtools::document("retractionwatch"); devtools::install("retractionwatch", upgrade=FALSE, quiet=TRUE)'

test:
	Rscript -e 'devtools::test("retractionwatch")'
	Rscript -e 'print(covr::package_coverage("retractionwatch"))'

# Regenerate NAMESPACE and man/*.Rd from the roxygen comments in
# retractionwatch/R/ (run it after adding, renaming or documenting a function,
# instead of editing NAMESPACE by hand), then check that the package loads
# and its tests pass
generate-documentation:
	Rscript -e 'devtools::document("retractionwatch")'
	Rscript -e 'devtools::test("retractionwatch")'

preview:
	quarto preview

render:
	quarto render

publish:
	quarto publish gh-pages

run-comparison:
	quarto render retraction-analysis.qmd
	quarto render retraction-analysis-library.qmd
	Rscript compare_checkpoints.R

# Simulate the GitHub Actions workflow by running
# .github/workflows/quarto-publish.yml with act (https://github.com/nektos/act)
# in an ubuntu-latest Docker container (Docker must be running).
# The workflow runs on a fresh clone of the last commit (HEAD), so untracked or
# gitignored local files (e.g. data/) are missing, just like on GitHub.
# Uncommitted changes are NOT tested: commit first (no need to push).
# The workflow file itself is taken from the working copy. Under act the
# publish step is skipped (if: !env.ACT) and the site is only rendered.
# Optionally export GITHUB_TOKEN to avoid GitHub API rate limits in setup steps.
CI_DIR ?= $(CURDIR)/.ci-local
ACT ?= $(or $(shell command -v act),$(CI_DIR)/bin/act)
ACT_IMAGE ?= catthehacker/ubuntu:act-latest

$(CI_DIR)/bin/act:
	mkdir -p "$(CI_DIR)/bin"
	curl -fsSL "https://github.com/nektos/act/releases/latest/download/act_$$(uname -s)_$$(uname -m | sed 's/aarch64/arm64/').tar.gz" \
		| tar -xz -C "$(CI_DIR)/bin" act

ci-local: $(ACT)
	@docker info > /dev/null 2>&1 || { echo "Docker is not running: start Docker Desktop first"; exit 1; }
	rm -rf "$(CI_DIR)/repo"
	git clone --quiet --no-local "$(CURDIR)" "$(CI_DIR)/repo"
	git -C "$(CI_DIR)/repo" remote set-url origin "$$(git remote get-url origin)"
	"$(ACT)" push \
		--directory "$(CI_DIR)/repo" \
		--workflows "$(CURDIR)/.github/workflows/quarto-publish.yml" \
		--platform ubuntu-latest=$(ACT_IMAGE) \
		--container-architecture linux/amd64 \
		--env RUNNER_ARCH=X64 \
		$(if $(GITHUB_TOKEN),--secret GITHUB_TOKEN)

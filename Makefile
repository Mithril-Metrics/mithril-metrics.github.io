# Makefile — shortcuts for agecon-cases. Run from the repository root.
# Recipes must be indented with a TAB.

CASE     ?=
SLUG     ?=
TITLE    ?=
REGION   ?= Global
METHOD   ?= Descriptive
EMPLOYER ?= WB
SPEC     ?=

.PHONY: help new run data figures report site preview check test install docs clean-case

help:
	@echo "make install                      install acutils from ./acutils"
	@echo "make test                         run acutils unit tests"
	@echo "make new SLUG=x TITLE='...' REGION=EU METHOD=SFA EMPLOYER=OECD"
	@echo "make check SPEC=tools/specs/x.yml 2-minute feasibility check"
	@echo "make run CASE=cases/YYYY-MM-slug  scripts 01-04 + render report"
	@echo "make report CASE=...              render report only (blog + brief + LinkedIn txt)"
	@echo "make site                         render the whole blog locally (updates _freeze/)"
	@echo "make preview                      live preview of the blog"

install:
	Rscript -e 'devtools::document("acutils"); devtools::install_local("acutils", force = TRUE, upgrade = "never")'

docs:
	Rscript -e 'devtools::document("acutils")'

test:
	Rscript -e 'devtools::test("acutils")'

new:
	@test -n "$(SLUG)" || (echo "SLUG is required" && exit 1)
	Rscript new_case.R $(SLUG) --title "$(TITLE)" --region "$(REGION)" --method "$(METHOD)" --employer "$(EMPLOYER)"

check:
	@test -n "$(SPEC)" || (echo "SPEC is required" && exit 1)
	Rscript tools/feasibility_check.R $(SPEC)

data:
	@test -n "$(CASE)" || (echo "CASE is required" && exit 1)
	cd $(CASE) && Rscript 01_get_data.R && Rscript 02_clean.R

run:
	@test -n "$(CASE)" || (echo "CASE is required" && exit 1)
	cd $(CASE) && Rscript 01_get_data.R && Rscript 02_clean.R && Rscript 03_analyse.R && Rscript 04_figures.R
	quarto render $(CASE)/report.qmd

figures:
	@test -n "$(CASE)" || (echo "CASE is required" && exit 1)
	cd $(CASE) && Rscript 03_analyse.R && Rscript 04_figures.R

report:
	@test -n "$(CASE)" || (echo "CASE is required" && exit 1)
	quarto render $(CASE)/report.qmd

site:
	quarto render

preview:
	quarto preview

clean-case:
	@test -n "$(CASE)" || (echo "CASE is required" && exit 1)
	rm -rf $(CASE)/output/cache $(CASE)/report_files

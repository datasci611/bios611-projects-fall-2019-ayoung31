pull-pandas:
	docker pull amancevice/pandas:0.25.2-slim && touch $@


data/cleaned.csv: scripts/wrangling.py pull-pandas
	docker run -it --rm -v $(shell pwd):/workdir -w /workdir \
		amancevice/pandas:0.25.2-slim python scripts/wrangling.py

pull-tidyverse:
	docker pull rocker/tidyverse && touch $@

results/report.html: results/report.Rmd data/cleaned.csv
	docker run -it --rm -v $(shell pwd):/workdir -w /workdir \
		-w /workdir rocker/tidyverse \
		Rscript -e "library(rmarkdown); rmarkdown::render('results/report.Rmd', 'html_document')"


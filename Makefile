
.PHONY: all clean plots

all: report.html report.pdf

plots:
	mkdir -p plots
	Rscript scripts/player_cluster.r
	Rscript scripts/player_rank.r
	Rscript scripts/miscluster_analysis.r

report.html: report.Rmd plots
	Rscript -e "rmarkdown::render('report.Rmd', output_format = 'html_document')"

report.pdf: report.Rmd plots
	Rscript -e "rmarkdown::render('report.Rmd', output_format = 'pdf_document')"

clean:
	rm -f report.html report.pdf

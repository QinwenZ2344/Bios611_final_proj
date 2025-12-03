
.PHONY: all clean plots

all: report.html report.pdf

plots:
	mkdir -p plots
	Rscript scripts/player_cluster.r
	Rscript scripts/player_rank.r
	Rscript scripts/miscluster_analysis.r

report.html: report.Rmd plots
	Rscript -e "rmarkdown::render('report.Rmd', output_format = 'html_document')"

report.pdf: report.html scripts/html2pdf.r
	Rscript scripts/html2pdf.r

clean:
	rm -f report.html report.pdf

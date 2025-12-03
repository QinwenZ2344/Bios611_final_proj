
.PHONY: all clean plots

all: report.html report.pdf

plots:
	cd /work && Rscript scripts/player_cluster.r
	cd /work && Rscript scripts/player_rank.r
	cd /work && Rscript scripts/miscluster_analysis.r

report.html: report.Rmd plots
	cd /work && Rscript -e "rmarkdown::render('report.Rmd', output_format = 'html_document')"

report.pdf: report.html scripts/html2pdf.r
	cd /work && Rscript scripts/html2pdf.r

clean:
	rm -f report.html report.pdf

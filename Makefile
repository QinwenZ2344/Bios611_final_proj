report.html: report.Rmd
	Rscript -e "rmarkdown::render('report.Rmd', output_format = 'html_document')"

.PHONY: clean
clean:
	rm -f report.html


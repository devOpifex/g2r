build:
				Rscript -e "packer::bundle_prod()"
				Rscript -e "devtools::document()"
				Rscript -e "devtools::test()"
				Rscript -e "devtools::check()"
				Rscript -e "devtools::install()"

init: 
				Rscript -e "packer::npm_install()"
				Rscript -e "packer::bundle_prod()"
				Rscript -e "devtools::install()"

site:
				Rscript -e "packer::bundle_prod()"
				Rscript -e "devtools::install()"
				Rscript -e "pkgdown::build_site()"

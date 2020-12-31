build:
				Rscript -e "packer::bundle_prod()"
				Rscript -e "devtools::document()"
				Rscript -e "devtools::check()"
				Rscript -e "devtools::install()"

init: 
				Rscript -e "packer::npm_install()"

site:
				Rscript -e "pkgdown::build_site()"

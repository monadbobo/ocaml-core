((language_defs
  ((ml ((comment_style (multi_line "(*" "*)"))))))
 (env
  ((project_name   Variantslib)
   (copyright_year 2011)
   (additional_header)
   (additional_copyright)))
 (header_template_file ../header.template)
 (actions
  ((header ml **.ml{,i})
   (keep {myocamlbuild.ml,setup.ml})
  (drop {**/,}{.hgignore.in,OMakefile})
  (drop {release.sexp}))))

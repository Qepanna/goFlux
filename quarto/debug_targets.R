source('_generate.R')

# Extract metadata
current <- metadata_from_repo()

# Show target mapping for import functions
import_funcs <- current[grepl("^import", current$function_name), ]
print(table(import_funcs$qmd_target))
print(head(import_funcs[, c('function_name', 'qmd_target')]))

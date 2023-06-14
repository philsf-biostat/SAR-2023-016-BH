# setup -------------------------------------------------------------------
# system.time(source("scripts/input-raw.R")) # run only once (~9 min)

# results -----------------------------------------------------------------
source('scripts/input.R', encoding = 'UTF-8') |> suppressMessages()
source('scripts/describe.R', encoding = 'UTF-8') |> suppressMessages()
source('scripts/modeling.R', encoding = 'UTF-8') |> suppressMessages()
source('scripts/inference.R', encoding = 'UTF-8') |> suppressMessages()
source('scripts/plots.R', encoding = 'UTF-8') |> suppressMessages()
# source('scripts/plots-save.R', encoding = 'UTF-8') |> suppressMessages() ## only manual!
# source('scripts/tables-save.R', encoding = 'UTF-8') |> suppressMessages() ## only manual!
# source('scripts/model-diagnostics.R', encoding = 'UTF-8') |> suppressMessages()

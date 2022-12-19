#use the function ftable() to create a pivot table

df <- read.table(text =
                   "  Function    SB 'Country Region' '+1 Function' '+1 SB' '+1 Country Region'
       ENG  SB10             AMER           ENG    SB10                AMER
        IT  SB07             EMEA            IT    SB07                EMEA
       QLT  SB05             EMEA           QLT    SB05                EMEA
       MFG  SB07             EMEA           MFG    SB07                EMEA
       MFG  SB04             EMEA           MFG    SB05                EMEA
       SCM  SB08             EMEA           SCM    SB08                EMEA",
                 stringsAsFactors = FALSE, header = TRUE, check.names = FALSE)

foo <- ftable(df, row.vars = c(3, 1, 2), col.vars = c(6, 4, 5))

as.matrix(foo)[apply(foo, 1, any), apply(foo, 2, any)]



install.packages("rpivotTable")
rpivotTable::rpivotTable(df, rows = c("Country Region", "Function", "SB"), cols = c("+1 Country Region", "+1 Function", "+1 SB"))

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

train.agg <-
          train[,.(
            TripType = Mode(TripType),
            Weekday = Mode(Weekday),
            Upc = Mode(Upc),
            FinelineNumber = Mode(FinelineNumber),
            ScanCount = mean(ScanCount, na.rm = T),
            DepartmentDescription = Mode(DepartmentDescription)
          ), VisitNumber]

test.agg <-
  test[,.(
    Weekday = Mode(Weekday),
    Upc = Mode(Upc),
    FinelineNumber = Mode(FinelineNumber),
    ScanCount = mean(ScanCount, na.rm = T),
    DepartmentDescription = Mode(DepartmentDescription)
  ), VisitNumber]

saveRDS(train.agg, "train_agg.RDS")
saveRDS(test.agg, "test_agg.RDS")

write.csv(train.agg, "train_agg.csv", row.names = F)
write.csv(test.agg, "test_agg.csv", row.names = F)
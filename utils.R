  require(datimvalidation)
  datimvalidation::loadSecrets("/opt/dhis2/dish.json")
  foo<-getValidOperatingUnits()
  ous<-setNames(foo$id,foo$name)

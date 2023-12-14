shorten.str = function(str,len=100, marker = "...") {
  #restore.point("shorten.str")
  if (length(str)==0) return(str)
  #str[is.na(str)] = ""
  is.bigger = nchar(str)>len
  is.bigger[is.na(is.bigger)] = FALSE

  str[is.bigger] = paste0(substring(str[is.bigger],1,len-nchar(marker)),marker)
  str
}


str.left.of = function (str, pattern, ..., not.found = str)
{
  pos = regexpr(pattern, str, fixed=TRUE)
  res = substring(str, 1, pos - 1)
  rows = pos == -1
  if (length(not.found)<length(str)) not.found = rep(not.found, length(str))
  res[rows] = not.found[rows]
  res
}

#' Get transactions for an address
#'
#' Get normal, internal, ERC-20, or ERC-721 transactions for an Ethereum address.
#'
#' @param address Character. A single ethereum address as a character string (40
#'   hexadecimal characters prepended by '0x').
#' @param api_key An Etherscan API key (see Details).
#' @param internal Logical. Should normal (`FALSE`, default) or internal
#'   (`TRUE`) transactions be queried? _Deprecated. Please use `type` instead._
#' @param type The type of transaction to query. One of `'normal'`,
#'   `'internal'`, `'ERC20'`, or `'ERC721'`.
#' @param startblock Starting block for transaction query. Default is 0.
#' @param endblock Ending block for transaction query. Default is 999999999.
#' @param network Ethereum network to use. One of `'mainnet'` (default),
#'   `'ropsten'`, `'rinkeby'`, `'kovan'`, or `'goerli'`.
#' @param first_page Logical. Should only the first page of results (up to
#'   10,000 transactions) be returned.
#' @param no_errors Logical. Should unsuccessful transactions be omitted
#'   (`FALSE`, default)?
#' @param quiet Logical. Suppress messages? Default is `FALSE`.
#' @return A `tbl_df` with transaction details. Columns vary depending on
#'   `type`.
#' @details `get_txs` uses the Etherscan API to source information about
#'   transactions to and from an Ethereum address. Register for an API key at
#'   the [Etherscan Developer APIs page](https://etherscan.io/apis).
#' @section Warning:
#' As per the Etherscan documentation, _the Etherscan Ethereum Developer APIs
#' are provided as a community service and without warranty, so please just use
#' what you need and no more. They support both GET/POST requests and a rate
#' limit of 5 requests/sec._
#' @keywords Ethereum, transaction, blockchain, cryptocurrency, crypto, ETH
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr filter as_tibble mutate select matches %>%
#' @importFrom lubridate with_tz now
#' @importFrom gmp as.bigz
#' @export
get_txs <- function(address, api_key, internal=FALSE,
                    type=c('normal', 'internal', 'ERC20', 'ERC721'),
                    startblock=0, endblock=999999999, network='mainnet',
                    first_page=FALSE, no_errors=TRUE,
                    quiet=FALSE) {
  if(as.numeric(startblock) > as.numeric(endblock))
    stop('endblock cannot be less than startblock')
  if (isTRUE(internal)) {
    warning("argument `internal` is deprecated; please use `type` instead.",
            call. = FALSE)
    type <- 'internal'
  } else {
    type <- match.arg(type)
  }
  address <- tolower(address)
  if(missing(api_key)) stop('API Key required. See https://etherscan.io/apis')
  network <- match.arg(network, c('mainnet', 'ropsten', 'rinkeby', 'kovan', 'goerli'))
  config <- list(address=address, type=type, network=network, no_errors=no_errors)
  network <- ifelse(network=='mainnet', '', paste0('-', network))
  txtype <- switch(
    type, normal='txlist', internal='txlistinternal',
    ERC20='tokentx', ERC721='tokennfttx')
  .get_txs <- function(network, txtype, address, sort, api_key, startblock,
                       endblock) {
    j <- jsonlite::fromJSON(sprintf(
      'http://api%s.etherscan.io/api?module=account&action=%s&address=%s&startblock=%s&endblock=99999999&sort=%s&apikey=%s',
      network, txtype, address, startblock, sort, api_key))
    if(j$status != '1') {
      if(j$message == 'No transactions found') {
        warning('No transactions found for address: ', address, call.=FALSE)
        return(NULL)
      } else {
        stop('Invalid address', call. = FALSE)
      }
    }
    j <- j$result
    switch(type,
           normal={
             j %>%
               dplyr::mutate(
                 timeStamp=as.numeric(timeStamp),
                 timeStamp=as.POSIXct(timeStamp, origin='1970-01-01'),
                 blockNumber=as.numeric(blockNumber),
                 nonce=as.numeric(nonce),
                 value=as.numeric(value),
                 transactionIndex=as.numeric(transactionIndex),
                 value_eth=value/1e18,
                 gas=as.numeric(gas),
                 gasPrice=as.numeric(gasPrice),
                 gasPrice_gwei=gasPrice/1e9,
                 isError=as.numeric(isError),
                 cumulativeGasUsed=as.numeric(cumulativeGasUsed),
                 gasUsed=as.numeric(gasUsed),
                 confirmations=as.numeric(confirmations)
               ) %>%
               dplyr::as_tibble()
           },
           internal={
             j %>%
               dplyr::mutate(
                 timeStamp=as.numeric(timeStamp),
                 timeStamp=as.POSIXct(timeStamp, origin='1970-01-01'),
                 blockNumber=as.numeric(blockNumber),
                 value=as.numeric(value),
                 value_eth=value/1e18,
                 gas=as.numeric(gas),
                 isError=as.numeric(isError),
                 gasUsed=as.numeric(gasUsed)
               ) %>%
               dplyr::as_tibble()
           },
           ERC20={
             d <- j %>%
               dplyr::mutate(
                 timeStamp=as.numeric(timeStamp),
                 timeStamp=as.POSIXct(timeStamp, origin='1970-01-01'),
                 blockNumber=as.numeric(blockNumber),
                 nonce=as.numeric(nonce),
                 tokenDecimal=as.numeric(tokenDecimal),
                 transactionIndex=as.numeric(transactionIndex),
                 gas=as.numeric(gas),
                 gasPrice=as.numeric(gasPrice),
                 gasPrice_gwei=gasPrice/1e9,
                 gasUsed=as.numeric(gasUsed),
                 cumulativeGasUsed=as.numeric(cumulativeGasUsed),
                 confirmations=as.numeric(confirmations)
               )
             d$value <- gmp::as.bigz(d$value)
             dplyr::as_tibble(d)
           },
           ERC721={
             j %>%
               dplyr::mutate(
                 timeStamp=as.numeric(timeStamp),
                 timeStamp=as.POSIXct(timeStamp, origin='1970-01-01'),
                 blockNumber=as.numeric(blockNumber),
                 nonce=as.numeric(nonce),
                 tokenID=as.integer(tokenID),
                 tokenDecimal=as.numeric(tokenDecimal),
                 transactionIndex=as.numeric(transactionIndex),
                 gas=as.numeric(gas),
                 gasPrice=as.numeric(gasPrice),
                 gasPrice_gwei=gasPrice/1e9,
                 gasUsed=as.numeric(gasUsed),
                 cumulativeGasUsed=as.numeric(cumulativeGasUsed),
                 confirmations=as.numeric(confirmations)
               ) %>%
               dplyr::as_tibble()
           }
    ) %>%
      dplyr::mutate(timeStamp=lubridate::with_tz(timeStamp, 'UTC'))
  }

  if(isTRUE(first_page)) {
    if(!quiet) message('Getting transactions...')
    txs <- .get_txs(network=network, txtype=txtype, address=address,
                    startblock=startblock, endblock=endblock, sort='desc',
                    api_key=api_key)
    if(is.null(txs)) return(NULL)
    txs <- dplyr::select(txs, dplyr::matches('^((?!confirmations).)*$', perl=T))
  } else {
    if(!quiet) message('Getting transactions...')
    txs <- .get_txs(network, txtype, address, 0, 'asc', api_key)
    if(is.null(txs)) return(NULL)
    txs <- dplyr::select(txs, dplyr::matches('^((?!confirmations).)*$', perl=T))
    n <- nrow(txs)
    while(n == 10000) {
      if(!quiet) message('Getting more transactions...')
      .txs <- .get_txs(network, txtype, address, max(txs$blockNumber), 'asc', api_key) %>%
        dplyr::select(dplyr::matches('^((?!confirmations).)*$', perl=T))
      n <- nrow(.txs)
      txs <- rbind(txs, .txs)
    }
  }
  now <- lubridate::now(tzone='UTC')
  txs <- unique(txs)
  if(isTRUE(no_errors) && type %in% c('normal', 'internal'))
    txs <- dplyr::filter(txs, isError=='0')
  attr(txs, 'address') <- config$address
  attr(txs, 'type') <- config$type
  attr(txs, 'network') <- config$network
  attr(txs, 'no_errors') <- config$no_errors
  attr(txs, 'last_updated') <- now
  txs
}

#' Summarize ETH flow for an address
#'
#' Summarize inflow and outflow of ETH from an address.
#'
#' @param address Character. A single externally owned account (user Ethereum
#'   address).
#' @param api_key An Etherscan API key (see Details).
#' @param get_names Logical. Get contract names for verified contracts?
#' @return A summary of inflow and outflow of ETH from an address.
#' @keywords Ethereum, contract, blockchain, cryptocurrency, crypto, ETH
#' @importFrom dplyr across arrange bind_rows desc filter group_by mutate select summarise
#' @importFrom tidyr pivot_wider
#' @importFrom magrittr %>%
#' @export
in_out <- function(address, api_key, get_names=FALSE) {
  address <- tolower(address)
  txs <- get_txs(address, api_key)
  txs_i <- get_txs(address, api_key, type='internal')
  froms <- dplyr::bind_rows(
    list(normal=dplyr::select(txs, address=from, value_eth),
         internal=dplyr::select(txs_i, address=from, value_eth)),
    .id='type'
  ) %>% dplyr::filter(address != !!address)
  tos <- dplyr::select(txs, address=to, value_eth) %>%
    dplyr::filter(address != !!address)
  flow <- dplyr::bind_rows(list(eth_in=froms, eth_out=tos), .id='direction') %>%
    dplyr::group_by(direction, address) %>%
    dplyr::summarise(value=sum(value_eth), .groups='drop') %>%
    tidyr::pivot_wider(names_from=direction, values_from=value) %>%
    dplyr::mutate(dplyr::across(eth_in:eth_out, ~ifelse(is.na(.x), 0, .x)),
                  net=eth_in - eth_out,
                  roi_pct=eth_in/eth_out*100) %>%
    dplyr::arrange(dplyr::desc(net))

  if(isTRUE(get_names)) flow$name <- get_name(flow$address, api_key)

  flow
}

#' Reset LDAP authentication
#' 
#' @param private Private list from R6 object
#' @param self Self from R6 object
#' 
#' @keywords internal
ldap_reset <- function(
  private,
  self
){
  private$authenticated <- F
  private$authenticated_user <- NULL
  private$authenticated_until <- NULL
  self
}

#' Simple bind
#' 
#' Perform a simple LDAP bind operation
#' 
#' @param self Self from R6 object
#' @param private Private list from R6 object
#' @param user User to authenticate
#' @param pw Password to authenticate
#' @param type Type of object to authenticate with - one of either `cn` or `uid`
#' @param timeout Timeout for authentication
#' 
#' @keywords internal
ldap_bind <- function(
  self,
  private,
  dn,
  pw,
  timeout
){
  # reset the authentication
  ldap_reset(private, self)
  
  # build the DN for binding
  bind_dn <- dn #'ldap_bind_dn(user, private$base_dn, type)
  
  # perform the bind
  r <- ldapr_bind_s(
    private$handle, 
    bind_dn,
    pw
  )
  
  # update self
  private$authenticated <- T
  private$authenticated_user <- dn
  private$authenticated_until <- get_timeout(timeout)
  self
}


# unbind
ldap_unbind <- function(
    private,
    self
){
  ldapr_unbind(private$handle)
  # update self
  private$authenticated <- F
  private$authenticated_user <- NULL
  private$authenticated_until <- NULL
  self
}


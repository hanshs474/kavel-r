kavel_base <- function() getOption("kavel.base_url", "https://www.kavel.ai")

kavel_key <- function(api_key) {
  if (!is.null(api_key) && nzchar(api_key)) return(api_key)
  env <- Sys.getenv("KAVEL_API_KEY")
  if (nzchar(env)) env else NULL
}

kavel_anon_id <- function() {
  paste0("r-", paste(sprintf("%02x", sample(0:255, 8, replace = TRUE)), collapse = ""))
}

# Every failure carries a class to branch on: kavel_quota (wait, or pass a
# key), kavel_rejected (reword the prompt), kavel_sign_in, kavel_auth,
# kavel_timeout.
kavel_stop <- function(kind, msg) {
  stop(structure(class = c(paste0("kavel_", kind), "kavel_error", "error", "condition"),
                 list(message = paste("kavel:", msg), call = NULL)))
}

kavel_request <- function(url, headers, body = NULL) {
  h <- new_handle()
  handle_setheaders(h, .list = headers)
  handle_setopt(h, timeout = 60)
  if (!is.null(body)) {
    handle_setheaders(h, .list = c(headers, "Content-Type" = "application/json"))
    handle_setopt(h, postfields = body)
  }
  res <- curl_fetch_memory(url, handle = h)
  env <- fromJSON(rawToChar(res$content), simplifyVector = FALSE)
  # Refusals answer 200 with code -1 and a human message.
  if (!identical(as.integer(env$code), 0L)) {
    msg <- if (is.null(env$message)) "request refused" else env$message
    low <- tolower(msg)
    kind <- if (grepl("invalid api key", low, fixed = TRUE)) "auth"
      else if (grepl("insufficient credits", low, fixed = TRUE)) "quota"
      else if (grepl("sign in|subscription", low)) "sign_in"
      else "other"
    kavel_stop(kind, msg)
  }
  env$data
}

kavel_run <- function(payload, api_key, poll_seconds, timeout_seconds) {
  key <- kavel_key(api_key)
  auth <- if (is.null(key)) c("x-anon-id" = kavel_anon_id()) else c(Authorization = paste("Bearer", key))
  deadline <- Sys.time() + timeout_seconds
  d <- kavel_request(paste0(kavel_base(), "/api/ai/generate"), auth,
                     toJSON(payload, auto_unbox = TRUE))
  # The quota wall answers 200 with code 0 and wall: true.
  if (isTRUE(d$wall)) {
    if (identical(d$reason, "anon_ip_daily")) kavel_stop("quota", "this machine has used its free credits for today")
    if (identical(d$reason, "anon_unmetered_video")) kavel_stop("sign_in", "this needs a signed-in account")
    kavel_stop("quota", "the free allowance does not cover this run")
  }
  if (is.null(d$id)) kavel_stop("other", "the service returned no task id")
  task <- d$id
  repeat {
    if (Sys.time() > deadline) kavel_stop("timeout", "no image before the deadline")
    Sys.sleep(poll_seconds)
    # Free runs only start when a poll crosses the end of the queue, so a
    # dropped poll is not a failed job: keep going.
    p <- tryCatch(
      if (is.null(key)) {
        kavel_request(paste0(kavel_base(), "/api/ai/anon-query?taskId=", curl_escape(task),
                             "&provider=kie&mediaType=image"), auth)
      } else {
        kavel_request(paste0(kavel_base(), "/api/ai/query"), auth,
                      toJSON(list(taskId = task), auto_unbox = TRUE))
      },
      kavel_error = function(e) NULL, error = function(e) NULL)
    if (is.null(p)) next
    if (length(p$cleanImages)) return(list(url = p$cleanImages[[1]], watermarked = FALSE))
    if (length(p$images)) {
      return(list(url = p$images[[1]],
                  watermarked = length(p$watermarked) > 0 && isTRUE(p$watermarked[[1]])))
    }
    if (isTRUE(p$status %in% c("failed", "error"))) kavel_stop("rejected", "prompt refused; reword it")
  }
}

#' Generate an image from a prompt
#'
#' Calls the free anonymous tier of Kavel unless an API key is given or
#' \code{KAVEL_API_KEY} is set.
#'
#' @param prompt What to draw. Naming the light, the material and the
#'   composition moves the result far more than adding adjectives.
#' @param aspect_ratio One of "1:1", "16:9", "9:16", "4:3", "3:4".
#' @param api_key Optional key from \url{https://www.kavel.ai/settings/apikeys}.
#' @param model Engine override, honoured only with a key.
#' @param poll_seconds How often the job is polled.
#' @param timeout_seconds Deadline for the whole call.
#' @return A list with \code{url} (a permanent CDN link) and \code{watermarked}.
#' @examples
#' \dontrun{
#' kavel_generate("matte black ceramic mug on pale oak, soft window light")
#' }
#' @export
kavel_generate <- function(prompt, aspect_ratio = "1:1", api_key = NULL, model = NULL,
                           poll_seconds = 5, timeout_seconds = 360) {
  if (!nzchar(trimws(prompt))) kavel_stop("other", "prompt is required")
  key <- kavel_key(api_key)
  kavel_run(list(provider = "kie", mediaType = "image",
                 model = if (!is.null(key) && !is.null(model)) model else "kavel-image-v1",
                 scene = "text-to-image", prompt = prompt,
                 options = list(aspect_ratio = aspect_ratio)),
            api_key, poll_seconds, timeout_seconds)
}

#' Edit an existing image
#'
#' An edit costs more than the free allowance, so without a key this signals
#' a \code{kavel_quota} condition before anything is charged.
#'
#' @param source_url A publicly reachable http(s) url.
#' @param instruction What to change. Say what must stay as well.
#' @inheritParams kavel_generate
#' @return A list with \code{url} and \code{watermarked}.
#' @export
kavel_edit <- function(source_url, instruction, api_key = NULL, model = NULL,
                       poll_seconds = 5, timeout_seconds = 360) {
  if (!grepl("^https?://", source_url)) kavel_stop("other", "source_url must be a public http(s) url")
  key <- kavel_key(api_key)
  kavel_run(list(provider = "kie", mediaType = "image",
                 model = if (!is.null(key) && !is.null(model)) model else "nano-banana-2-lite",
                 scene = "image-to-image", prompt = instruction,
                 options = list(image_input = list(source_url))),
            api_key, poll_seconds, timeout_seconds)
}

#' Free allowance for a fresh anonymous client id
#'
#' @return A list with \code{remaining} and \code{grant}. Free to call.
#' @export
kavel_credits <- function() {
  d <- kavel_request(paste0(kavel_base(), "/api/ai/anon-credits"), c("x-anon-id" = kavel_anon_id()))
  list(remaining = d$remaining, grant = d$grant)
}

# kavel

Generate AI images from R with **no API key and no account**.

```r
install.packages("kavel", repos = c("https://hanshs474.r-universe.dev", "https://cloud.r-project.org"))

library(kavel)
img <- kavel_generate("matte black ceramic mug on pale oak, soft window light from the left",
                      aspect_ratio = "16:9")
img$url          # https://cdn.kavel.ai/uploads/kie/image/....webp
img$watermarked  # TRUE on the free tier
```

Every other image client wants a key from OpenAI, fal or Replicate before it runs once. This one
calls the anonymous tier of [Kavel](https://www.kavel.ai/?utm_source=runiverse&utm_medium=package),
an online AI image and video studio, so the first call works from a fresh R session.

## Conditions you can catch

```r
tryCatch(kavel_generate("a lighthouse at dusk, oil painting"),
  kavel_quota    = function(e) message("free allowance spent for today"),
  kavel_rejected = function(e) message("reword the prompt"))
```

Classes: `kavel_quota`, `kavel_rejected`, `kavel_sign_in`, `kavel_auth`, `kavel_timeout`, all
inheriting from `kavel_error`.

## The free tier

`kavel_credits()` reads what a fresh client id is granted, and the call costs nothing. On 2026-10-02 the
grant was 5 credits, one generated image spent all five, and one IP got two images that day. Free
output is 1K and watermarked.

## Editing a photo

```r
kavel_edit("https://example.com/portrait.jpg",
           "shoulder-length layered haircut, keep the same face, skin and lighting",
           api_key = Sys.getenv("KAVEL_API_KEY"))
```

An edit costs more than the free grant, so it needs a key from
[kavel.ai/settings/apikeys](https://www.kavel.ai/settings/apikeys?utm_source=runiverse&utm_medium=package).
Without one it signals `kavel_quota` before anything is charged.

## License

MIT. Generated with [Kavel AI](https://www.kavel.ai/?utm_source=runiverse&utm_medium=package).

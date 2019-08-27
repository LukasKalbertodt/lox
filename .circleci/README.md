# CI stuff

## Docker image

(This is basically just a reminder to myself as I always forget how to use Docker)

To update Rust in the image, run:

```
docker build --no-cache -t lukaskalbertodt/loc-ci:yyyy-mm-dd  # replace with date
docker push lukaskalbertodt/loc-ci:yyyy-mm-dd
```

You can check the images you built with `docker images`.

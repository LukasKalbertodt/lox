# CI stuff

## Docker image

(This is basically just a reminder to myself as I always forget how to use Docker)

To update Rust in the image, run:

```
# replace with date
docker build --no-cache -t lukaskalbertodt/lox-ci:yyyy-mm-dd .
docker push lukaskalbertodt/lox-ci:yyyy-mm-dd
```

You can check the images you built with `docker images`.

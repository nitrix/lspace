echo Cleaning directory...
rm -f client
rm -f dispatch
rm -f launcher
rm -f server

echo Building projects...
go list github.com/nitrix/lspace/... | xargs -n 1 go build

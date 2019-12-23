FROM golang:latest

RUN go get -u github.com/motemen/gore/cmd/gore
RUN go get -u github.com/mdempsky/gocode
RUN go get -u github.com/k0kubun/pp

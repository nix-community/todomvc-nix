package app

import (
	"context"
	"encoding/json"
	"errors"
	"net/http"
	"strconv"

	"github.com/go-kit/kit/log"
	"github.com/go-kit/kit/transport"
	httptransport "github.com/go-kit/kit/transport/http"
	"github.com/gorilla/mux"
)

var (
	ErrBadRouting = errors.New("inconsistent mapping between route and handler (programmer error)")
	ErrNoInt      = errors.New("input parameter isn't integer")
)

func MakeHTTPHandler(s Service, logger log.Logger) http.Handler {
	r := mux.NewRouter()
	e := MakeServerEndpoints(s)
	options := []httptransport.ServerOption{
		httptransport.ServerErrorHandler(transport.NewLogErrorHandler(logger)),
		httptransport.ServerErrorEncoder(encodeError),
	}

	r.Methods("GET").Path("/").Handler(httptransport.NewServer(
		e.AliveEndpoint,
		decodeEmptyRequest,
		encodeResponse(http.StatusOK),
		options...,
	))
	r.Methods("GET").Path("/todos").Handler(httptransport.NewServer(
		e.GetTodosEndpoint,
		decodeEmptyRequest,
		encodeResponse(http.StatusOK),
		options...,
	))
	r.Methods("GET").Path("/todos/{id}").Handler(httptransport.NewServer(
		e.GetTodoEndpoint,
		decodeGetTodoRequest,
		encodeResponse(http.StatusOK),
		options...,
	))
	r.Methods("POST").Path("/todos").Handler(httptransport.NewServer(
		e.PostTodoEndpoint,
		decodePostTodoRequest,
		encodeResponse(http.StatusCreated),
		options...,
	))
	r.Methods("DELETE").Path("/todos").Handler(httptransport.NewServer(
		e.DeleteTodosEndpoint,
		decodeEmptyRequest,
		encodeResponse(http.StatusOK),
		options...,
	))
	r.Methods("DELETE").Path("/todos/{id}").Handler(httptransport.NewServer(
		e.DeleteTodoEndpoint,
		decodeDeleteTodoRequest,
		encodeResponse(http.StatusOK),
		options...,
	))
	r.Methods("PUT", "PATCH").Path("/todos/{id}").Handler(httptransport.NewServer(
		e.PatchTodoEndpoint,
		decodePatchTodoRequest,
		encodeResponse(http.StatusOK),
		options...,
	))

	return accessControl(r)
}

func decodePostTodoRequest(_ context.Context, r *http.Request) (request interface{}, err error) {
	var req postTodoRequest
	if e := json.NewDecoder(r.Body).Decode(&req.Todo); e != nil {
		return nil, e
	}

	return req, nil
}

func decodeEmptyRequest(_ context.Context, r *http.Request) (request interface{}, err error) {
	return nil, nil
}

func decodeGetTodoRequest(_ context.Context, r *http.Request) (request interface{}, err error) {
	vars := mux.Vars(r)
	id, ok := vars["id"]
	if !ok {
		return nil, ErrBadRouting
	}

	idAsInt, err := strconv.Atoi(id)
	if err != nil {
		return nil, ErrNoInt
	}

	return getTodoRequest{ID: idAsInt}, nil
}

func decodePatchTodoRequest(_ context.Context, r *http.Request) (request interface{}, err error) {
	vars := mux.Vars(r)
	id, ok := vars["id"]
	if !ok {
		return nil, ErrBadRouting
	}

	var t Todo
	if err := json.NewDecoder(r.Body).Decode(&t); err != nil {
		return nil, err
	}

	idAsInt, err := strconv.Atoi(id)
	if err != nil {
		return nil, ErrNoInt
	}

	return patchTodoRequest{
		ID:   idAsInt,
		Todo: t,
	}, nil
}

func decodeDeleteTodoRequest(_ context.Context, r *http.Request) (request interface{}, err error) {
	vars := mux.Vars(r)
	id, ok := vars["id"]
	if !ok {
		return nil, ErrBadRouting
	}

	idAsInt, err := strconv.Atoi(id)
	if err != nil {
		return nil, ErrNoInt
	}

	return deleteTodoRequest{ID: idAsInt}, nil
}

func encodeResponse(statusCode int) func(ctx context.Context, w http.ResponseWriter, response interface{}) error {
	return func(ctx context.Context, w http.ResponseWriter, response interface{}) error {
		w.Header().Set("Content-Type", "application/json; charset=utf-8")
		w.WriteHeader(statusCode)
		return json.NewEncoder(w).Encode(response)
	}
}

func encodeError(_ context.Context, err error, w http.ResponseWriter) {
	if err == nil {
		panic("encodeError with nil error")
	}

	w.Header().Set("Content-Type", "application/json; charset=utf-8")
	w.WriteHeader(codeFrom(err))
	json.NewEncoder(w).Encode(map[string]interface{}{
		"error": err.Error(),
	})
}

func codeFrom(err error) int {
	switch err {
	case ErrNotFound:
		return http.StatusNotFound
	case ErrAlreadyExists, ErrInconsistentIDs, ErrNoInt:
		return http.StatusBadRequest
	default:
		return http.StatusInternalServerError
	}
}

func accessControl(h http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Access-Control-Allow-Origin", "*")
		w.Header().Set("Access-Control-Allow-Methods", "GET, POST, DELETE, PUT, PATCH")
		w.Header().Set("Access-Control-Allow-Headers", "Origin, Content-Type")

		if r.Method == "OPTIONS" {
			return
		}

		h.ServeHTTP(w, r)
	})
}

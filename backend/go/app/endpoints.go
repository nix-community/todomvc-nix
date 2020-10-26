package app

import (
	"context"

	"github.com/go-kit/kit/endpoint"
)

type Endpoints struct {
	AliveEndpoint       endpoint.Endpoint
	GetTodosEndpoint    endpoint.Endpoint
	GetTodoEndpoint     endpoint.Endpoint
	PostTodoEndpoint    endpoint.Endpoint
	DeleteTodosEndpoint endpoint.Endpoint
	DeleteTodoEndpoint  endpoint.Endpoint
	PatchTodoEndpoint   endpoint.Endpoint
}

func MakeServerEndpoints(s Service) Endpoints {
	return Endpoints{
		AliveEndpoint:       MakeAliveEndpoint(s),
		PostTodoEndpoint:    MakePostTodoEndpoint(s),
		GetTodoEndpoint:     MakeGetTodoEndpoint(s),
		GetTodosEndpoint:    MakeGetTodosEndpoint(s),
		PatchTodoEndpoint:   MakePatchTodoEndpoint(s),
		DeleteTodoEndpoint:  MakeDeleteTodoEndpoint(s),
		DeleteTodosEndpoint: MakeDeleteTodosEndpoint(s),
	}
}

func MakeAliveEndpoint(_ Service) endpoint.Endpoint {
	return func(ctx context.Context, request interface{}) (response interface{}, err error) {
		return map[string]string{"status": "OK"}, nil
	}
}

func MakePostTodoEndpoint(s Service) endpoint.Endpoint {
	return func(ctx context.Context, request interface{}) (response interface{}, err error) {
		req := request.(postTodoRequest)
		t, e := s.InsertTodo(ctx, req.Todo)
		return postTodoResponse{Todo: t}, e
	}
}

func MakeGetTodoEndpoint(s Service) endpoint.Endpoint {
	return func(ctx context.Context, request interface{}) (response interface{}, err error) {
		req := request.(getTodoRequest)
		t, e := s.GetTodo(ctx, req.ID)
		return getTodoResponse{Todo: t}, e
	}
}

func MakeGetTodosEndpoint(s Service) endpoint.Endpoint {
	return func(ctx context.Context, request interface{}) (response interface{}, err error) {
		t, e := s.GetTodos(ctx)
		return getTodosResponse(t), e
	}
}

func MakePatchTodoEndpoint(s Service) endpoint.Endpoint {
	return func(ctx context.Context, request interface{}) (response interface{}, err error) {
		req := request.(patchTodoRequest)
		t, e := s.UpdateTodo(ctx, req.ID, req.Todo)
		return patchTodoResponse{Todo: t}, e
	}
}

func MakeDeleteTodoEndpoint(s Service) endpoint.Endpoint {
	return func(ctx context.Context, request interface{}) (response interface{}, err error) {
		req := request.(deleteTodoRequest)
		e := s.DeleteTodo(ctx, req.ID)
		return nil, e
	}
}

func MakeDeleteTodosEndpoint(s Service) endpoint.Endpoint {
	return func(ctx context.Context, request interface{}) (response interface{}, err error) {
		e := s.DeleteTodos(ctx)
		return nil, e
	}
}

type postTodoRequest struct {
	Todo
}

type postTodoResponse struct {
	Todo
}

type getTodoRequest struct {
	ID int
}

type getTodoResponse struct {
	Todo
}

type getTodosResponse []Todo

type patchTodoRequest struct {
	ID int
	Todo
}

type patchTodoResponse struct {
	Todo
}

type deleteTodoRequest struct {
	ID int
}

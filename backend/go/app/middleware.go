package app

import (
	"context"
	"time"

	"github.com/go-kit/kit/log"
)

type Middleware func(Service) Service

func LoggingMiddleware(logger log.Logger) Middleware {
	return func(next Service) Service {
		return &loggingMiddleware{
			next:   next,
			logger: logger,
		}
	}
}

type loggingMiddleware struct {
	next   Service
	logger log.Logger
}

func (mw loggingMiddleware) Clear() (err error) {
	defer func(begin time.Time) {
		mw.logger.Log("method", "Clear", "took", time.Since(begin), "err", err)
	}(time.Now())
	return mw.next.Clear()
}

func (mw loggingMiddleware) InsertTodo(ctx context.Context, t Todo) (t2 Todo, err error) {
	defer func(begin time.Time) {
		mw.logger.Log("method", "InsertTodo", "took", time.Since(begin), "err", err)
	}(time.Now())
	return mw.next.InsertTodo(ctx, t)
}

func (mw loggingMiddleware) GetTodo(ctx context.Context, id int) (t Todo, err error) {
	defer func(begin time.Time) {
		mw.logger.Log("method", "GetTodo", "id", id, "took", time.Since(begin), "err", err)
	}(time.Now())
	return mw.next.GetTodo(ctx, id)
}

func (mw loggingMiddleware) UpdateTodo(ctx context.Context, id int, t Todo) (t2 Todo, err error) {
	defer func(begin time.Time) {
		mw.logger.Log("method", "UpdateTodo", "id", id, "took", time.Since(begin), "err", err)
	}(time.Now())
	return mw.next.UpdateTodo(ctx, id, t)
}

func (mw loggingMiddleware) DeleteTodo(ctx context.Context, id int) (err error) {
	defer func(begin time.Time) {
		mw.logger.Log("method", "DeleteTodo", "id", id, "took", time.Since(begin), "err", err)
	}(time.Now())
	return mw.next.DeleteTodo(ctx, id)
}

func (mw loggingMiddleware) GetTodos(ctx context.Context) (t []Todo, err error) {
	defer func(begin time.Time) {
		mw.logger.Log("method", "GetTodos", "took", time.Since(begin), "err", err)
	}(time.Now())
	return mw.next.GetTodos(ctx)
}

func (mw loggingMiddleware) DeleteTodos(ctx context.Context) (err error) {
	defer func(begin time.Time) {
		mw.logger.Log("method", "DeleteTodos", "took", time.Since(begin), "err", err)
	}(time.Now())
	return mw.next.DeleteTodos(ctx)
}

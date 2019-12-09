package ${packageName}.presenter.impl;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import ${packageName}.presenter.BasePresenter;

/**
 * Abstract presenter implementation that contains base implementation for other presenters.
 * Subclasses must call super for all {@link BasePresenter} method overriding.
 */
public abstract class BasePresenterImpl<V> implements BasePresenter<V>
{
    /**
     * The view
     */
    @Nullable
    protected V mView;

    @Override
    public void onViewAttached(@NonNull V view)
    {
        mView = view;
    }


    @Override
    public void onStart(boolean viewCreated)
    {

    }

    @Override
    public void onStop()
    {

    }


    @Override
    public void onViewDetached()
    {
        mView = null;
    }

    @Override
    public void onPresenterDestroyed()
    {
        
    }
}

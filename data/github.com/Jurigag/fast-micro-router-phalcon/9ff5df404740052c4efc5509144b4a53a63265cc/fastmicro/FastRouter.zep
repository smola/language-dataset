namespace Fastmicro;

use Phalcon\Mvc\Router;
use Phalcon\Mvc\Router\Route;
use Phalcon\Mvc\Router\GroupInterface;
use Phalcon\DiInterface;
use Phalcon\Mvc\Router\Exception;
use Phalcon\Mvc\Router\RouteInterface;

class FastRouter extends Router
{
    /**
     * @var bool
     */
    protected build = false;

    /**
     * @var bool
     */
    protected buildFromCache = false;

    /**
     * @var array
     */
    protected groupClassNames = [];

    /**
     * @var array
     */
    protected callbackRoutes = [];

    /**
     * @var bool
     */
    protected cache = false;

    /**
     * @var string
     */
    protected cacheObjectService;

    protected cacheSimpleService { get };

    /**
     * @var string
     */
    protected key;

    /**
     * @var array
     */
    protected patterns = [];

    /**
     * @var array
     */
    protected patternsIds = [];

    /**
     * @var string
     */
    protected globalPattern;

    public function attach(<RouteInterface> route, position = Router::POSITION_LAST)
    {
        var compiledPattern, realPattern;

        if (!this->buildFromCache) {
            /**
             * Get compiled pattern
             */
            let compiledPattern = route->getCompiledPattern();

            /**
             * If we have # at start we assume it's regexp, it's how phalcon does it at least
             */
            if starts_with(compiledPattern, "#") {
                let realPattern = str_replace(["#^", "$#u"], "", compiledPattern);
                this->addPattern(realPattern, route->getRouteId());
            } else {
                /**
                 * If it's not regexp add it as direct match
                 */
                let this->_routes[compiledPattern][] = route;
            }

            let this->_routes[route->getRouteId()] = route;
        }

        return this;
    }

    public function add(pattern, paths = null, httpMethods = null, position = Router::POSITION_LAST)
    {
        var route;

        let route = new Route(pattern, paths, httpMethods);

        if (!this->buildFromCache) {
            this->attach(route, position);
        }

        /**
         * We need to return even dummy route if someone is using setName
         */

        return route;
    }

    protected function addPattern(pattern, routeId)
    {
        var newPattern;

        if !isset this->patternsIds[pattern] {
            let this->patternsIds[pattern] = [];
        }
        let this->patternsIds[pattern][] = routeId;
        let routeId = implode(',', this->patternsIds[pattern]);
        let newPattern = pattern."(*:".routeId.")";
        let this->patterns[pattern] = newPattern;
        let this->globalPattern = "#^(?|".implode('|', this->patterns).")$#x";
    }

    public function handle(uri = null)
    {
        var realUri, handledUri, eventsManager, eventManagerIsObject, matches, routes, routeId, dependencyInjector, routeFound,
            request, currentHostName, parts, params, methods, hostname, regexHostname, matched, paths, converters, part, position,
            converter, notFoundPaths, route, vnamespace, module, action, controller, paramsStr, strParams, beforeMatch, matchPosition;

        if (!this->build) {
            this->build();
        }

        if (!uri) {
            let realUri = this->getRewriteUri();
        } else {
            let realUri = uri;
        }

        if (this->_removeExtraSlashes && realUri != "/") {
            let handledUri = rtrim(realUri, '/');
        } else {
            let handledUri = realUri;
        }

        let eventsManager = this->_eventsManager;
        let eventManagerIsObject = typeof eventsManager == "object";

        if (eventManagerIsObject) {
            eventsManager->fire("router:beforeCheckRoutes", this);
        }

        let matches = null;

        if !fetch routes, this->_routes[handledUri] {
            if (preg_match(this->globalPattern, handledUri, matches)) {
               let routeId = matches["MARK"];
               if (memstr(routeId,",")) {
                    let routes = [this->_routes[routeId]];
               } else {
                    let routeId = explode(',', routeId);
                    let routes = this->getRoutesByIds(routeId);
               }
            } else {
                let routes = [];
            }
        }

        let dependencyInjector = this->_dependencyInjector,
            routeFound = false,
            request = null,
            currentHostName = null,
            parts = [],
            params = [];

        if (dependencyInjector) {
            let request = dependencyInjector->get("request");
            let currentHostName = request->getHttpHost();
        }

        for route in routes {
            /**
             * For later phalcon just add method to match single route in router class and set needed properties
             */
            /**
             * We will fire beforeCheckRoute here because all $routes are already potential correct routes, only method, hostname or beforeMatch can be wrong
             */
            if (eventManagerIsObject) {
                eventsManager->fire("router:beforeCheckRoute", this, route);
            }

            let methods = route->getHttpMethods();
            if (methods && request->isMethod(methods, true) === false) {
                continue;
            }

            let hostname = route->getHostname();
            if (hostname) {
                if (!currentHostName) {
                    continue;
                }

                if (memstr(hostname, ")")) {
                    if (!memstr(hostname,"#")) {
                        let regexHostname = "#^".hostname;
                        if (!memstr(hostname, ":")) {
                            let regexHostname .= "(:[[:digit:]]+)?";
                        }
                        let regexHostname .= "$#i";
                    } else {
                        let regexHostname = hostname;
                    }
                    let matched = preg_match(regexHostname, currentHostName);
                } else {
                    let matched = currentHostName == hostname;
                }

                if (!matched) {
                    continue;
                }
            }

            let routeFound = true;

            if (eventManagerIsObject) {
                eventsManager->fire("router:matchedRoute", this, route);
            }

            let beforeMatch = route->getBeforeMatch();
            if (beforeMatch) {
                if (!is_callable(beforeMatch)) {
                    throw new Exception("Before-Match callback is not callable in matched route");
                }

                /**
                 * Phalcon uses call_user_func_array for no obvious reason
                 */
                let routeFound = {beforeMatch}(handledUri, route, this);
            }

            if (routeFound) {
                let paths = route->getPaths();
                let parts = paths;

                if (typeof matches == "array") {
                    let converters = route->getConverters();

                    for part, position in paths {
                        if (typeof part != "string") {
                            throw new Exception("Wrong key in paths: ".part);
                        }

						if typeof position != "string" && typeof position != "integer" {
							continue;
						}

						if fetch matchPosition, matches[position] {

							/**
							 * Check if the part has a converter
							 */
							if typeof converters == "array" {
								if fetch converter, converters[part] {
									let parts[part] = call_user_func_array(converter, [matchPosition]);
									continue;
								}
							}

							/**
							 * Update the parts if there is no converter
							 */
							let parts[part] = matchPosition;
						} else {

							/**
							 * Apply the converters anyway
							 */
							if typeof converters == "array" {
								if fetch converter, converters[part] {
									let parts[part] = call_user_func_array(converter, [position]);
								}
							} else {

								/**
								 * Remove the path if the parameter was not matched
								 */
								if typeof position == "integer" {
									unset parts[part];
								}
							}
						}
					}

					/**
					 * Update the matches generated by preg_match
					 */
					let this->_matches = matches;
				}

				let this->_matchedRoute = route;
				break;
			}
		}

		/**
		 * Update the wasMatched property indicating if the route was matched
		 */
		if routeFound {
			let this->_wasMatched = true;
		} else {
			let this->_wasMatched = false;
		}

		/**
		 * The route wasn't found, try to use the not-found paths
		 */
		if !routeFound {
			let notFoundPaths = this->_notFoundPaths;
			if notFoundPaths !== null {
				let parts = Route::getRoutePaths(notFoundPaths),
					routeFound = true;
			}
		}

		/**
		 * Use default values before we overwrite them if the route is matched
		 */
		let this->_namespace = this->_defaultNamespace,
			this->_module = this->_defaultModule,
			this->_controller = this->_defaultController,
			this->_action = this->_defaultAction,
			this->_params = this->_defaultParams;

		if routeFound {

			/**
			 * Check for a namespace
			 */
			if fetch vnamespace, parts["namespace"] {
				if !is_numeric(vnamespace) {
					let this->_namespace = vnamespace;
				}
				unset parts["namespace"];
			}

			/**
			 * Check for a module
			 */
			if fetch module, parts["module"] {
				if !is_numeric(module) {
					let this->_module = module;
				}
				unset parts["module"];
			}

			/**
			 * Check for a controller
			 */
			if fetch controller, parts["controller"] {
				if !is_numeric(controller) {
					let this->_controller = controller;
				}
				unset parts["controller"];
			}

			/**
			 * Check for an action
			 */
			if fetch action, parts["action"] {
				if !is_numeric(action) {
					let this->_action = action;
				}
				unset parts["action"];
			}

			/**
			 * Check for parameters
			 */
			if fetch paramsStr, parts["params"] {
				if typeof paramsStr == "string" {
					let strParams = trim(paramsStr, "/");
					if strParams !== "" {
						let params = explode("/", strParams);
					}
				}

				unset parts["params"];
			}

			if count(params) {
				let this->_params = array_merge(params, parts);
			} else {
				let this->_params = parts;
			}
		}

		if typeof eventsManager == "object" {
			eventsManager->fire("router:afterCheckRoutes", this);
		}
    }

    /**
     * Return routes by route id
     *
     * @param array $keys
     * @return array
     */
    protected function getRoutesByIds(array keys)
    {
        var result, key;

        let result = [];
        for key in keys {
            let result[] = this->_routes[key];
        }

        return result;
    }

    public function getRouteById(id)
    {
        var route;

        if fetch route, this->_routes[id] {
            return route;
        }

        return null;
    }

    public function useCache(cache, key = "router", cacheObjectService = "cache", cacheSimpleService = "cache")
    {
        let this->cache = cache;
        let this->cacheObjectService = cacheObjectService;
        let this->cacheSimpleService = cacheSimpleService;
        let this->key = key;

        return this;
    }

    public function addCallbackRoutes(callable callback)
    {
        let this->callbackRoutes[] = callback;

        return this;
    }

    public function mountGroupClassName(className)
    {
        let this->groupClassNames[] = className;

        return this;
    }

    protected function build()
    {
        var eventsManager, eventsManagerIsObject, dependencyInjector, cacheSimpleService, data, cacheObjectService;

        let eventsManager = this->_eventsManager;
        let eventsManagerIsObject = typeof eventsManager == "object";

        if (eventsManagerIsObject) {
            eventsManager->fire("router:beforeBuild", this);
        }

        /**
         * This method loads cached router or builds routes using callbacks and group classes names
         */
        if (this->cache) {
            let dependencyInjector = this->_dependencyInjector;

            let cacheObjectService = dependencyInjector->get(this->cacheObjectService);
            let cacheSimpleService = dependencyInjector->get(this->cacheSimpleService);
            this->loadRoutesByCallbackAndGroupClassName();
            cacheObjectService->save("PHFR_ROUTES_".this->key, this->_routes);
            let data = [
                "globalPattern": this->globalPattern,
                "patternsIds": this->patternsIds,
                "patterns": this->patterns
            ];
            cacheSimpleService->save("PHFR_".this->key, data);
        } else {
            this->loadRoutesByCallbackAndGroupClassName();
        }

        if (eventsManagerIsObject) {
            eventsManager->fire("router:afterBuild", this);
        }

        let this->build = true;
    }

    protected function loadRoutesByCallbackAndGroupClassName()
    {
        var callbackRoute, groupClassName, group;

        for callbackRoute in this->callbackRoutes {
            {callbackRoute}(this);
        }
        for groupClassName in this->groupClassNames {
            let group = new {groupClassName};
            this->mount(group);
        }
    }

    public function mount(<GroupInterface> group)
    {
        var eventsManager, groupRoutes, beforeMatch, hostname, route;

        if (!this->buildFromCache) {
            /** @var Manager $eventsManager */
            let eventsManager = this->_eventsManager;

            if (typeof eventsManager == "object") {
                eventsManager->fire("router:beforeMount", this, group);
            }

            /** @var Route[] $groupRoutes */
            let groupRoutes = group->getRoutes();

            if (!count(groupRoutes)) {
                throw new Exception("The group of routes does not contain any routes");
            }

            let beforeMatch = group->getBeforeMatch();
            let hostname = group->getHostname();

            if (beforeMatch || hostname) {
                for route in groupRoutes {
                    if (beforeMatch) {
                        route->beforeMatch(beforeMatch);
                    }
                    if (hostname) {
                        route->setHostname(hostname);
                    }
                    this->attach(route);
                }
            }
        }

        return this;
    }

    /**
     * Builds router from cache
     */
    protected function buildFromCache()
    {
        var eventsManager, dependencyInjector, cacheObjectService, cacheSimpleService, data, eventsManagerIsObject;

        if (this->cache) {
            /** @var Manager $eventsManager */
            let eventsManager = this->_eventsManager;
            let eventsManagerIsObject = typeof eventsManager == "object";

            if (eventsManagerIsObject) {
                eventsManager->fire("router:beforeBuild", this);
            }

            /** @var Di $dependencyInjector */
            let dependencyInjector = this->_dependencyInjector;

            /** @var BackendInterface $cacheService */
            let cacheObjectService = dependencyInjector->get(this->cacheObjectService);
            let cacheSimpleService = dependencyInjector->get(this->cacheSimpleService);
            if (cacheObjectService->exists("PHFR_ROUTES_".this->key) && cacheSimpleService->exists("PHFR_".this->key)) {
                let this->_routes = cacheObjectService->get("PHFR_ROUTES_".this->key);
                let data = cacheSimpleService->get("PHFR_".this->key);
                let this->globalPattern = data["globalPattern"];
                let this->patternsIds = data["patternsIds"];
                let this->patterns = data["patterns"];
                let this->buildFromCache = true;
                let this->build = true;
                if (eventsManagerIsObject) {
                    eventsManager->fire("router:afterBuild", this);
                }
            }
        }
    }

    public function setDI(<DiInterface> dependencyInjector)
    {
        parent::setDI(dependencyInjector);
        /**
         * As soon as router is resolved from di so setDi is called try to build router from cache
         */
        this->buildFromCache();
    }

    public function isBuild()
    {
        return this->build;
    }

    public function isBuildFromCache()
    {
        return this->buildFromCache;
    }

    public function isCache()
    {
        return this->cache;
    }
}

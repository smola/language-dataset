/*
 * This file is part of the Symfony package.
 *
 * (c) Fabien Potencier <fabien@symfony.com>
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */

namespace Symfony\Component\Asset;

use Symfony\Component\Asset\Context\ContextInterface;
use Symfony\Component\Asset\VersionStrategy\VersionStrategyInterface;
use Symfony\Component\Asset\Exception\InvalidArgumentException;
use Symfony\Component\Asset\Exception\LogicException;

/**
 * Package that adds a base URL to asset URLs in addition to a version.
 *
 * The package allows to use more than one base URLs in which case
 * it randomly chooses one for each asset; it also guarantees that
 * any given path will always use the same base URL to be nice with
 * HTTP caching mechanisms.
 *
 * When the request context is available, this package can choose the
 * best base URL to use based on the current request scheme:
 *
 *  * For HTTP request, it chooses between all base URLs;
 *  * For HTTPs requests, it chooses between HTTPs base URLs and relative protocol URLs
 *    or falls back to any base URL if no secure ones are available.
 *
 * @author Fabien Potencier <fabien@symfony.com>
 */
class UrlPackage extends Package
{
    private $baseUrls = [];
    private $sslPackage;

    /**
     * @param string|string[]          $baseUrls        Base asset URLs
     * @param VersionStrategyInterface $versionStrategy The version strategy
     * @param ContextInterface|null    $context         Context
     */
    public function __construct(var baseUrls, <VersionStrategyInterface> versionStrategy, <ContextInterface> context = null) -> void
    {
        parent::__construct(versionStrategy, context);

        var sslUrls, baseUrl;

        if ( typeof baseUrls != "array" ) {
            var tmpBaseUrl;
            let tmpBaseUrl  = baseUrls;
            let baseUrls    = [];
            let baseUrls[0] = tmpBaseUrl;
        }
        
        if !baseUrls {
            throw new LogicException("You must provide at least one base URL.");
        }

        for baseUrl in baseUrls {
            let this->baseUrls[] = rtrim(baseUrl, "/");
        }

        let sslUrls = this->getSslUrls(baseUrls);

        if sslUrls && baseUrls !== sslUrls {
            let this->sslPackage = new self(sslUrls, versionStrategy);
        }
    }

    /**
     * {@inheritdoc}
     */
    public function getUrl(string! path) -> string
    {
        var url;

        if this->isAbsoluteUrl(path) {
            return path;
        }

        if null !== this->sslPackage && this->getContext()->isSecure() {

            return this->sslPackage->getUrl(path);
        }

        let url = this->getVersionStrategy()->applyVersion(path);

        if url && "/" != substr(url, 0, 1) {
            let url = "/" . $url;
        }

        return this->getBaseUrl(path).url;
    }

    /**
     * Returns the base URL for a path.
     *
     * @param string $path
     *
     * @return string The base URL
     */
    public function getBaseUrl(string! path) -> string
    {
        if 1 === count(this->baseUrls) {
            return this->baseUrls[0];
        }

        return this->baseUrls[this->chooseBaseUrl(path)];
    }

    /**
     * Determines which base URL to use for the given path.
     *
     * Override this method to change the default distribution strategy.
     * This method should always return the same base URL for a given path.
     *
     * @param string $path
     *
     * @return string The base URL for the given path
     */
    protected function chooseBaseUrl(string! path) -> string
    {
        return fmod(hexdec(substr(hash("sha256", path), 0, 10)), count(this->baseUrls));
    }

    private function getSslUrls(array urls) -> array
    {
        var url, sslUrls = [];

        for url in urls {

            if "https://" === substr(url, 0, 8) || "//" === substr(url, 0, 2) {
                let sslUrls[] = url;
            } elseif "http://" !== substr(url, 0, 7) {
                throw new InvalidArgumentException(sprintf("\"%s\" is not a valid URL", url));
            }
        }

        return sslUrls;
    }
}
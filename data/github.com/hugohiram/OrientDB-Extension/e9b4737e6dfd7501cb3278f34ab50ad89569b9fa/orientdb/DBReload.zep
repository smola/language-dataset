/**
 * OrientDB DBReload class
 * Reloads database information. Available since 1.0rc4.
 *
 * @author Hugo Hiram <hugo@hugohiram.com>
 * @copyright Hugo Hiram 2014
 * @license MIT License (MIT) https://github.com/hugohiram/OrientDB-Extension/blob/master/LICENSE
 * @link https://github.com/hugohiram/OrientDB-Extension
 * @package OrientDB
 */

namespace Orientdb;

use Orientdb\Exception\OrientdbException;

/**
 * DBReload() Operation for OrientDB
 *
 * @author Hugo Hiram <hugo@hugohiram.com>
 * @package OrientDB
 * @subpackage Operation
 */
class DBReload extends OperationAbstract
{

	/**
	 * Orientdb\DBOpen constructor
	 *
	 * @param object parent object of caller class
	 */
	public function __construct(parent)
	{
		//echo __CLASS__;
		let this->parent = parent;
		let this->socket = parent->socket;

		let this->operation = OperationAbstract::REQUEST_DB_RELOAD;
	}

	/**
	 * Main method to run the operation
	 * 
	 * @return boolean
	 */
	public function run() -> boolean
	{
		this->prepare();
		this->execute();
		let this->response = this->parseResponse();

		return this->response;
	}

	/**
	 * Prepare the parameters
	 * 
	 * @return void
	 */
	protected function prepare() -> void
	{
		this->resetRequest();
		this->addByte(chr(this->operation));

		let this->session = this->parent->getSession();
		this->addInt(this->session);
	}

	/**
	 * Parse the response from the socket
	 * 
	 * @return array
	 */
	protected function parseResponse() -> array
	{
		var session, status;
		var numClusters, cluster, clusters;

		let status = this->readByte(this->socket);
		let session = this->readInt(this->socket);
		this->parent->setSession(session);

		//if (status == (chr(OperationAbstract::STATUS_SUCCESS))) {
		if (ord(status) == OperationAbstract::STATUS_SUCCESS) {
			let numClusters = this->readShort(this->socket);
			let clusters = [];

			var pos;
			for pos in range(1, numClusters) {
				let cluster = [
					"name": this->readString(this->socket),
					"id":   this->readShort(this->socket),
					"type": (this->parent->protocolVersion < 24)? this->readString(this->socket) : null,
					"datasegmentid": (this->parent->protocolVersion < 24)? this->readShort(this->socket)  : null
				];

				let clusters[] = cluster;
			}

			return ["numClusters":numClusters, "clusters":clusters];
		}
		else {
			this->handleException();
		}

		return [];
	}
}
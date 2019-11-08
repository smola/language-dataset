namespace Yenc;

class yEnc
{
	const VERSION = "1.3.0";

	/*
	 * Text of the most recent error message (if any).
	 */
	public lastError;

	/**
	 * Filename to use for encoding or decoding.
	 */
	protected filename {
		get, set, toString
	};

	public function decode(string! encodedText, boolean ignoreErrors = false) -> string|boolean
	{
		if ignoreErrors {
			return this->decodeDirty(encodedText);
		}
		var dummy, entry;
		array text = [], matches = [];
		int arraySize, code, index = 0, lineSize, headSize, part = 0, tailPart, tailSize;
		string begin = "0", crc, decoded = "", end = "0", head, line, message = "", tail, total = "0";

		let this->filename = "";
		let this->lastError = "";

		let text = (array)explode("\r\n", trim(encodedText));

		let arraySize = count(text);
		if unlikely (arraySize < 3) {
			let this->lastError = "Data too short. There should be at least three lines.";
			return false;
		}

		let head = (string)array_shift(text);
		if preg_match(
			"#^=ybegin(?:\s+part=(?P<part>\d+)(?:\s+total=(?P<total>\d+)|)|)\s+line=(?P<line>\d+)\s+size=(?P<size>\d+)\s+name=(?P<name>[^ ]+).*$#i",
//" corrects colouring of strings in PHPS.
			head,
			matches
		) {
			let headSize = (int)matches["size"];
			let lineSize = (int)matches["line"];
			let this->filename = (string)matches["name"];
			if isset(matches["part"]) && matches["part"] != "" {
echo "Part: " . matches["part"] . "\n";
				let part = (int)matches["part"];
				let total = isset(matches["total"]) ? (int)matches["total"] : 0;

				let head = (string)array_shift(text);
				if unlikely !preg_match(
					"#=ypart\s+begin=(?P<begin>\d+)\send=(?P<end>\d+)#i",
					head,
					matches
				) {
					//=ypart begin=1 end=100000
					let this->lastError = "Part info missing from multi-part message! This indicates probable corruption." . PHP_EOL;
					return false;
				} else {
					let begin = (int)matches["begin"];
					let end = (int)matches["end"];
				}
			} else {
				let begin = 0;
				let end = 0;
				let part = 0;
				let total = 0;
			}

		} else {
			let this->lastError = "Failed to match head" . PHP_EOL . head;
			return false;
		}

		let tail = (string)array_pop(text);

		if preg_match(
			"#^=yend\s+size=(?P<size>\d+)\s+(?:crc32=(?P<crc>[a-f0-9]+)|part=(?P<parttail>\d+)\s+pcrc32=(?P<pcrc>[a-f0-9]+))$#i",
			tail,
			matches
		) {
			let tailSize = (int)matches["size"];
			let tailPart = !isset(matches["parttail"]) ? 0 : (int)matches["parttail"];

			if unlikely (part != tailPart) {
				let this->lastError = "Multi-part part numbers do not match. This is a violation of the yEnc specification and indicates probable corruption." . PHP_EOL;
				return false;
			}

			if (tailPart == 0) {
				// No tailPart means no Multi-part
				let crc = (string)matches["crc"];
			} else {
				let crc = (string)matches["pcrc"];
			}
		} else {
			let this->lastError = "Failed to match tail" . PHP_EOL . tail;
			return false;
		}

		// Make sure the prefix and suffix filesizes match up.
		if unlikely (tailPart == 0 && headSize != tailSize) {
			let dummy = headSize;
			let message = "Header/trailer file sizes do not match (" . (string)dummy . "/";
			let dummy = tailSize;
			let message .= (string)dummy . "). This is a violation of the yEnc specification and indicates probable corruption.";
			let this->lastError = message;

			return false;
		}

		for entry in text {
			let index = 0;
			let line = (string)entry;
			let lineSize = line->length();
			if unlikely lineSize == 0 {
				continue;
			}

			// Decode loop
			while index < lineSize {
				let dummy = line[index];
				let index++;
				let code = (int)dummy;
				if code == 61 { // '='
					if unlikely (lineSize <= index) {
						let this->lastError = "Last character of a line cannot be the escape character. The file is probably corrupt."
							 . PHP_EOL;
						return false;
					} else {
						let dummy = line[index];
						let index++;
						let code = ((int)dummy - 64);
						if code < 0 {
							let code += 256;
						}
					}
				}

				let code = (code - 42);
				if code < 0 {
					let code += 256;
				}

				//let dummy = chr(code);
				let decoded .= chr(code);
			}
		}

		// Make sure the decoded filesize is the same as the size specified in the tail, because mulit-parts use tail size.
		let headSize = decoded->length();
		if (tailSize != headSize) {
			let dummy = tailSize;
			let message = "Tail size (" . (string)dummy . ") and actual size (";
			let dummy = headSize;
			let message .= (string)dummy . ") do not match. The file is probably corrupt.";
			let this->lastError = message;

			return false;
		}

		// Check the CRC value
		let dummy = sprintf("%X", crc32(decoded));
		if !empty crc && (crc->upper() != (string)dummy) {
			let this->lastError = "CRC32 checksums do not match (" . crc->upper() . "/" . (string)dummy . "). The file is probably corrupt.";

			return false;
		}

		return decoded;
	}

  //
  // Decode encoded text ignoring all but most basic errors.
	//
	public function decodeDirty(string! encodedText) -> string|boolean
	{
		array text = [], matches = [];
		int code, index = 0, lineSize;
		string decoded = "", line;
		var dummy, entry;

		let matches = (array)explode("\r\n", encodedText);
		let code = 1;
		if (strpos(matches[1], "=ypart ") === 0) {
			let code = code + 1;
		}
		let text = (array)array_slice(matches, code, -1);

		if unlikely (count(text) < 1) {
			let this->lastError = "No text to decode!";
			return false;
		} else {

			for entry in text {
				let index = 0;
				let line = (string)entry;
				let lineSize = line->length();
				if unlikely lineSize == 0 {
					continue;
				}

				// Decode loop
				while index < lineSize {
					let dummy = line[index];
					let index++;
					let code = (int)dummy;
					if code == 61 { // '='
						if unlikely (lineSize <= index) {
							continue;
						} else {
							let dummy = line[index];
							let index++;
							let code = ((int)dummy - 64);
							if code < 0 {
								let code += 256;
							}
						}
					}

					let code = (code - 42);
					if code < 0 {
						let code += 256;
					}

					//let dummy = chr(code);
					let decoded .= chr(code);
				}
			}
		}

		return decoded;
	}

	public function encode(string! fileData, string! fileName, int! maxLineLen = 128) -> string|boolean
	{
		var dummy;
		array output = [];
		int charCount, code, index = 0;
		string encoded;

		let this->filename = fileName;
		let this->lastError = "";

		if (fileData->length() < 1) {
			throw new \UnexpectedValueException("There must be some content to encode.");
		}

		if (fileName->length() == 0) {
			throw new \UnexpectedValueException("Filename is required.");
		}

		if (maxLineLen < 1 || maxLineLen > 254) {
			throw new \RangeException("The maximum line length must be between 1 and 254 inclusive.");
		}

		let output[0] = (string)sprintf(
			"=ybegin line=%s size=%s name=%s",
			maxLineLen,
			fileData->length(),
			fileName
		);

		let charCount = maxLineLen;
		while index < fileData->length() {
			let dummy = fileData[index];
			let index++;
			let code = ((int)dummy + 42) % 256;

			switch (code) {
				case 0x00:	// null
				case 0x09:	// HT
				case 0x0A:	// LF
				case 0x0D:	// CR
				case 0x20:	// space
				case 0x3D:  // Including the escape character itself
					let encoded .= '=';	// Escape the the next character.
					let charCount--;
					let code = (code + 64) % 256;
					break;
// First or last column only.
//						break;
//				case 0x2E:  // Some unusual servers have problems with a full-stop in the first column
//						break;
				default:
					break;
			}

			let encoded .= chr(code);
			let charCount--;

			if charCount < 1 {
				let output[] = encoded;
				let encoded = "";
				let charCount = maxLineLen;
			}
		}
		let output[] = encoded;

		let output[] = (string)sprintf("=yend size=%d crc32=%x", fileData->length(), crc32(fileData));

		return implode("\r\n", output);
	}

	public static function version()
	{
		return yEnc::VERSION;
	}

	public function createTestString() -> string
	{
		//var dummy;
		string data = "";
		int ch;

		for ch in range(0, 255) {
			let data .= chr(ch);
			//echo ch, PHP_EOL;
		}

		return data;
	}

	protected function echoAsHex(string! line) {
		char ascii;
		var dummy;

		for ascii in line {
			echo ascii->toHex();
			echo ",";
		}
		let dummy = chr(8);
		echo (string)dummy . " " . PHP_EOL;
	}
}

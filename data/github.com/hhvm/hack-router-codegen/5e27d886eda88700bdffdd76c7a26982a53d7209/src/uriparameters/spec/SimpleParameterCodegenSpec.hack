/*
 *  Copyright (c) 2016-present, Facebook, Inc.
 *  All rights reserved.
 *
 *  This source code is licensed under the MIT license found in the
 *  LICENSE file in the root directory of this source tree.
 *
 */

namespace Facebook\HackRouter;

use type Facebook\HackRouter\UriParameterCodegenArgumentSpec as Args;

abstract class SimpleParameterCodegenSpec extends UriParameterCodegenSpec {
  const type TSimpleSpec = shape(
    'type' => string,
    'accessorSuffix' => string,
  );
  abstract protected static function getSimpleSpec(): self::TSimpleSpec;

  <<__Override>>
  final public static function getGetterSpec(
    RequestParameter $_,
  ): self::TSpec {
    $spec = static::getSimpleSpec();
    return shape(
      'type' => $spec['type'],
      'accessorSuffix' => $spec['accessorSuffix'],
      'args' => ImmVector {
        Args::ParameterName(),
      },
    );
  }

  <<__Override>>
  public static function getSetterSpec(
    UriParameter $_,
  ): self::TSpec {
    $spec = static::getSimpleSpec();
    return shape(
      'type' => $spec['type'],
      'accessorSuffix' => $spec['accessorSuffix'],
      'args' => ImmVector {
        Args::ParameterName(),
        Args::ParameterValue(),
      },
    );
  }
}

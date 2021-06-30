/*
 * Copyright 2020, 2021 KRR Oxford
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package uk.ac.ox.cs.rsacomb.util

/** Utility to allow hussle free version switching of blocks of code
  *
  * This allows for example testing different implementations of a
  * module or algorithm.
  */
trait Versioned[T] {

  /** Type of the returned versioned object */
  type Result

  /** Returns correct instance of the versioned object
    *
    * @param t object uniquely identifing the requested instance.
    */
  def apply(t: T): Result
}

// SPDX-License-Identifier: MIT
pragma solidity ^0.8.26;

import {Test} from "forge-std/Test.sol";
import {PlankDeployer, BuildOptions, Dependency} from "plank-foundry/PlankDeployer.sol";

abstract contract PlankTestBase is Test, PlankDeployer {
    error MissingDependency(string name);

    Dependency[] private deps;

    constructor() {
        deps.push(Dependency("std", "lib/plank-monorepo/std/"));
    }

    function addDep(string memory name, string memory path) internal {
        uint256 n = deps.length;
        for (uint256 i = 0; i < n; i++) {
            if (_nameEq(deps[i].name, name)) {
                deps[i].path = path;
                return;
            }
        }
        deps.push(Dependency(name, path));
    }

    function removeDep(string memory name) internal {
        uint256 n = deps.length;
        for (uint256 i = 0; i < n; i++) {
            if (_nameEq(deps[i].name, name)) {
                for (uint256 j = i; j + 1 < n; j++) {
                    deps[j] = deps[j + 1];
                }
                deps.pop();
                return;
            }
        }
        revert MissingDependency(name);
    }

    function plankOpts() internal view returns (BuildOptions memory opts) {
        opts = initBuildOptions().withBackend("sir");
        opts.dependencies = deps;
    }

    function deployPlank(string memory path) internal returns (address addr) {
        addr = plankDeployFFI(path, plankOpts());
    }

    function _nameEq(string memory a, string memory b) private pure returns (bool) {
        return keccak256(bytes(a)) == keccak256(bytes(b));
    }
}
